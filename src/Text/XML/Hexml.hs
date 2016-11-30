{-# LANGUAGE RecordWildCards #-}

-- | A module for first-approximation parsing of XML.
--   Note that entities, e.g. @&amp;@, are not expanded.
module Text.XML.Hexml(
    Document, Node, Attribute(..),
    documentParse, documentRender, documentNode,
    nodeName, nodeInner, nodeOuter,
    nodeAttributes, nodeChildren, nodeContents,
    nodeAttributeBy, nodeChildrenBy
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

foreign import ccall document_parse :: CString -> CInt -> IO (Ptr CDocument)
foreign import ccall document_free :: Ptr CDocument -> IO ()
foreign import ccall "&document_free" document_free_funptr :: FunPtr (Ptr CDocument -> IO ())
foreign import ccall document_render :: Ptr CDocument -> CString -> CInt -> IO CInt
foreign import ccall document_error :: Ptr CDocument -> IO CString
foreign import ccall document_node :: Ptr CDocument -> IO (Ptr CNode)

foreign import ccall node_children :: Ptr CDocument -> Ptr CNode -> Ptr CInt -> IO (Ptr CNode)
foreign import ccall node_attributes :: Ptr CDocument -> Ptr CNode -> Ptr CInt -> IO (Ptr CAttr)

foreign import ccall node_childBy :: Ptr CDocument -> Ptr CNode -> Ptr CNode -> CString -> CInt -> IO (Ptr CNode)
foreign import ccall node_attributeBy :: Ptr CDocument -> Ptr CNode -> CString -> CInt -> IO (Ptr CAttr)

-- | An XML document, created by 'documentParse'.
data Document = Document BS.ByteString (ForeignPtr CDocument)

data Node = Node BS.ByteString (ForeignPtr CDocument) (Ptr CNode)

data Attribute = Attribute BS.ByteString BS.ByteString deriving (Show, Eq, Ord)

instance Show Document where
    show d = "Document " ++ show (BS.unpack $ nodeOuter $ documentNode d)

instance Show Node where
    show d = "Node " ++ show (BS.unpack $ nodeOuter d)


documentParse :: BS.ByteString -> Either BS.ByteString Document
documentParse src = unsafePerformIO $ BS.unsafeUseAsCStringLen (src <> BS.singleton '\0') $ \(str, len) -> do
    doc <- document_parse str (fromIntegral len - 1)
    err <- document_error doc
    if err /= nullPtr then do
        bs <- BS.packCString =<< document_error doc
        document_free doc
        return $ Left bs
     else do
        doc <- newForeignPtr document_free_funptr doc
        return $ Right $ Document src doc

documentRender :: Document -> BS.ByteString
documentRender (Document _ doc) = unsafePerformIO $ withForeignPtr doc $ \d -> do
    i <- document_render d nullPtr 0
    BS.create (fromIntegral i) $ \ptr -> void $ document_render d (castPtr ptr) i


documentNode :: Document -> Node
documentNode d@(Document src doc) = unsafePerformIO $ withForeignPtr doc $ \d -> do
    Node src doc <$> document_node d

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
    return $ Attribute name val

nodeName :: Node -> BS.ByteString
nodeName = nodeBS 0

nodeInner :: Node -> BS.ByteString
nodeInner = nodeBS 1

nodeOuter :: Node -> BS.ByteString
nodeOuter = nodeBS 2

nodeContents :: Node -> [Either BS.ByteString Node]
nodeContents n@(Node src _ _) = f (strStart inner) outers
    where
        f i [] = string i (strEnd inner) ++ []
        f i ((x, n):xs) = string i (strStart x) ++ Right n : f (strEnd x) xs

        string start end | start == end = []
                         | otherwise = [Left $ applyStr src $ Str start (end - start)]
        inner = nodeStr 1 n
        outers = map (nodeStr 2 &&& id) $ nodeChildren n

nodeChildren :: Node -> [Node]
nodeChildren (Node src doc n) = unsafePerformIO $ withForeignPtr doc $ \d -> do
    alloca $ \count -> do
        res <- node_children d n count
        count <- fromIntegral <$> peek count
        return [Node src doc $ plusPtr res $ i*szNode | i <- [0..count-1]]

nodeAttributes :: Node -> [Attribute]
nodeAttributes (Node src doc n) = unsafePerformIO $ withForeignPtr doc $ \d -> do
    alloca $ \count -> do
        res <- node_attributes d n count
        count <- fromIntegral <$> peek count
        return [attrPeek src doc $ plusPtr res $ i*szAttr | i <- [0..count-1]]

nodeChildrenBy :: Node -> BS.ByteString -> [Node]
nodeChildrenBy (Node src doc n) str = go nullPtr
    where
        go old = unsafePerformIO $ withForeignPtr doc $ \d ->
            BS.unsafeUseAsCStringLen str $ \(bs, len) -> do
                r <- node_childBy d n old bs $ fromIntegral len
                return $ if r == nullPtr then [] else Node src doc r : go r

nodeAttributeBy :: Node -> BS.ByteString -> Maybe Attribute
nodeAttributeBy (Node src doc n) str = unsafePerformIO $ withForeignPtr doc $ \d ->
    BS.unsafeUseAsCStringLen str $ \(bs, len) -> do
        r <- node_attributeBy d n bs $ fromIntegral len
        return $ if r == nullPtr then Nothing else Just $ attrPeek src doc r
