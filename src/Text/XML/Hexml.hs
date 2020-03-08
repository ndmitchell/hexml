{-# LANGUAGE RecordWildCards, BangPatterns #-}

-- | A module for fast first-approximation parsing of XML.
--   Note that entities, e.g. @&amp;@, are not expanded.
module Text.XML.Hexml(
    Node, Attribute(..),
    parse, render,
    location, name, inner, outer,
    attributes, children, contents,
    attributeBy, childrenBy
    ) where

import Control.Applicative
import Control.Monad
import Data.Int
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal hiding (void)
import Foreign.ForeignPtr
import Foreign.Storable
import System.IO.Unsafe
import Data.Monoid
import Data.Tuple.Extra
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Internal as BS
import Prelude

data CDocument
data CNode
data CAttr

szAttr = 2 * sizeOf (undefined :: Str)
szNode = 5 * sizeOf (undefined :: Str)


data Str = Str {strStart :: {-# UNPACK #-} !Int32, strLength :: {-# UNPACK #-} !Int32} deriving Show

strEnd :: Str -> Int32
strEnd Str{..} = strStart + strLength

instance Storable Str where
    sizeOf _ = 8
    alignment _ = alignment (0 :: Int64)
    peek p = Str <$> peekByteOff p 0 <*> peekByteOff p 4
    poke p Str{..} = pokeByteOff p 0 strStart >> pokeByteOff p 4 strLength

foreign import ccall hexml_document_parse :: CString -> CInt -> IO (Ptr CDocument)
foreign import ccall hexml_document_free :: Ptr CDocument -> IO ()
foreign import ccall "&hexml_document_free" hexml_document_free_funptr :: FunPtr (Ptr CDocument -> IO ())
foreign import ccall hexml_node_render :: Ptr CDocument -> Ptr CNode -> CString -> CInt -> IO CInt
foreign import ccall unsafe hexml_document_error :: Ptr CDocument -> IO CString
foreign import ccall unsafe hexml_document_node :: Ptr CDocument -> IO (Ptr CNode)

foreign import ccall unsafe hexml_node_children :: Ptr CDocument -> Ptr CNode -> Ptr CInt -> IO (Ptr CNode)
foreign import ccall unsafe hexml_node_attributes :: Ptr CDocument -> Ptr CNode -> Ptr CInt -> IO (Ptr CAttr)

foreign import ccall unsafe hexml_node_child :: Ptr CDocument -> Ptr CNode -> Ptr CNode -> CString -> CInt -> IO (Ptr CNode)
foreign import ccall unsafe hexml_node_attribute :: Ptr CDocument -> Ptr CNode -> CString -> CInt -> IO (Ptr CAttr)

-- | A node in an XML document, created by 'parse', then calling functions such
--   as 'children' on that initial 'Node'.
data Node = Node BS.ByteString (ForeignPtr CDocument) (Ptr CNode)

-- | An XML attribute, comprising of a name and a value. As an example,
--   @hello=\"world\"@ would produce @Attribute \"hello\" \"world\"@.
data Attribute = Attribute
    {attributeName :: BS.ByteString
    ,attributeValue :: BS.ByteString
    } deriving (Show, Eq, Ord)

instance Show Node where
    show d = "Node " ++ show (BS.unpack $ outer d)


touchBS :: BS.ByteString -> IO ()
touchBS = touchForeignPtr . fst3 . BS.toForeignPtr


-- | Parse a ByteString as an XML document, returning a 'Left' error message, or a 'Right' document.
--   Note that the returned node will have a 'name' of @\"\"@, no 'attributes', and 'contents' as per the document.
--   Often the first child will be the @\<?xml ... ?\>@ element. For documents which comprise an XML node and a single
--   root element, use @'children' n !! 1@.
parse :: BS.ByteString -> Either BS.ByteString Node
parse src = do
    let src0 = src <> BS.singleton '\0'
    unsafePerformIO $ BS.unsafeUseAsCStringLen src0 $ \(str, len) -> do
        doc <- hexml_document_parse str (fromIntegral len - 1)
        err <- hexml_document_error doc
        if err /= nullPtr then do
            bs <- BS.packCString =<< hexml_document_error doc
            hexml_document_free doc
            pure $ Left bs
         else do
            node <- hexml_document_node doc
            doc <- newForeignPtr hexml_document_free_funptr doc
            pure $ Right $ Node src0 doc node

-- | Given a node, rerender it to something with an equivalent parse tree.
--   Mostly useful for debugging - if you want the real source document use 'outer' instead.
render :: Node -> BS.ByteString
render (Node src doc n) = unsafePerformIO $ withForeignPtr doc $ \d -> do
    i <- hexml_node_render d n nullPtr 0
    res <- BS.create (fromIntegral i) $ \ptr -> void $ hexml_node_render d n (castPtr ptr) i
    touchBS src
    pure res

applyStr :: BS.ByteString -> Str -> BS.ByteString
applyStr bs Str{..} = BS.take (fromIntegral strLength) $ BS.drop (fromIntegral strStart) bs

nodeStr :: Int -> Node -> Str
nodeStr i (Node src doc n) = unsafePerformIO $ withForeignPtr doc $ \_ -> peekElemOff (castPtr n) i

nodeBS :: Int -> Node -> BS.ByteString
nodeBS i node@(Node src doc n) = applyStr src $ nodeStr i node

attrPeek :: BS.ByteString -> ForeignPtr CDocument -> Ptr CAttr -> Attribute
attrPeek src doc a = unsafePerformIO $ withForeignPtr doc $ \_ -> do
    name <- applyStr src <$> peekElemOff (castPtr a) 0
    val  <- applyStr src <$> peekElemOff (castPtr a) 1
    pure $ Attribute name val

-- | Get the name of a node, e.g. @\<test /\>@ produces @\"test\"@.
name :: Node -> BS.ByteString
name = nodeBS 0

-- | Get the inner text, from inside the tag, e.g. @\<test /\>@ produces @\"\"@
--   and @\<test\>hello\</test\>@ produces @\"hello\"@.
--   The result will have identical layout/spacing to the source document.
inner :: Node -> BS.ByteString
inner = nodeBS 1

-- | Get the outer text, including the tag itself, e.g. @\<test /\>@ produces @\"\<test /\>\"@
--   and @\<test\>hello\</test\>@ produces @\"\<test\>hello\</test\>\"@.
--   The result will have identical layout/spacing to the source document.
outer :: Node -> BS.ByteString
outer = nodeBS 2

-- | Get the contents of a node, including both the content strings (as 'Left', never blank) and
--   the direct child nodes (as 'Right').
--   If you only want the child nodes, use 'children'.
contents :: Node -> [Either BS.ByteString Node]
contents n@(Node src _ _) = f (strStart inner) outers
    where
        f i [] = string i (strEnd inner) ++ []
        f i ((x, n):xs) = string i (strStart x) ++ Right n : f (strEnd x) xs

        string start end | start == end = []
                         | otherwise = [Left $ applyStr src $ Str start (end - start)]
        inner = nodeStr 1 n
        outers = map (nodeStr 2 &&& id) $ children n

-- | Get the direct child nodes of this node.
children :: Node -> [Node]
children (Node src doc n) = unsafePerformIO $ withForeignPtr doc $ \d ->
    alloca $ \count -> do
        res <- hexml_node_children d n count
        count <- fromIntegral <$> peek count
        pure [Node src doc $ plusPtr res $ i*szNode | i <- [0..count-1]]

-- | Get the attributes of this node.
attributes :: Node -> [Attribute]
attributes (Node src doc n) = unsafePerformIO $ withForeignPtr doc $ \d ->
    alloca $ \count -> do
        res <- hexml_node_attributes d n count
        count <- fromIntegral <$> peek count
        pure [attrPeek src doc $ plusPtr res $ i*szAttr | i <- [0..count-1]]

-- | Get the direct children of this node which have a specific name.
--   A more efficient version of:
--
-- > childrenBy p s = filter (\n -> name n == s) $ children p
childrenBy :: Node -> BS.ByteString -> [Node]
childrenBy (Node src doc n) str = go nullPtr
    where
        go old = unsafePerformIO $ withForeignPtr doc $ \d ->
            BS.unsafeUseAsCStringLen str $ \(bs, len) -> do
                r <- hexml_node_child d n old bs $ fromIntegral len
                touchBS src
                pure $ if r == nullPtr then [] else Node src doc r : go r

-- | Get the first attribute of this node which has a specific name, if there is one.
--   A more efficient version of:
--
-- > attributeBy n s = listToMaybe $ filter (\(Attribute a _) -> a == s $ attributes n
attributeBy :: Node -> BS.ByteString -> Maybe Attribute
attributeBy (Node src doc n) str = unsafePerformIO $ withForeignPtr doc $ \d ->
    BS.unsafeUseAsCStringLen str $ \(bs, len) -> do
        r <- hexml_node_attribute d n bs $ fromIntegral len
        touchBS src
        pure $ if r == nullPtr then Nothing else Just $ attrPeek src doc r

-- | Find the starting location of a node, the @<@ character.
--   The first character will be reported as @(line 1,column 1)@, because thats
--   how error messages typically do it.
location :: Node -> (Int, Int)
location n@(Node src _ _) = BS.foldl' f (pair 1 1) $ BS.take (fromIntegral i) src
    where
        pair !a !b = (a,b)

        i = strStart $ nodeStr 2 n
        f (!line, !col) c
            | c == '\n' = pair (line+1) 1
            | c == '\t' = pair line (col+8)
            | otherwise = pair line (col+1)
