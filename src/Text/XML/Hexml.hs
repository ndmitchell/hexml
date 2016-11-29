{-# LANGUAGE RecordWildCards #-}

module Text.XML.Hexml(
    Document, Node, Attribute(..),
    parse, render, rootNode,
    nodeName, nodeInner, nodeOuter,
    nodeAttributes, nodeChildren, nodeContents,
    nodeAttributeBy, nodeChildrenBy
    ) where

import Control.Monad
import Data.Int
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import System.IO.Unsafe
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Internal as BS

data CDocument
data CNode
data CAttr

data Str = Str {start :: {-# UNPACK #-} !Int32, length :: {-# UNPACK #-} !Int32}

instance Storable Str where
    sizeOf _ = 8
    alignment _ = alignment (0 :: Int64)
    peek p = Str <$> peekByteOff p 0 <*> peekByteOff p 4
    poke p Str{..} = pokeByteOff p 0 start >> pokeByteOff p 4 length

foreign import ccall document_parse :: CString -> CInt -> IO (Ptr CDocument)
foreign import ccall document_free :: Ptr CDocument -> IO ()
foreign import ccall "&document_free" document_free_funptr :: FunPtr (Ptr CDocument -> IO ())
foreign import ccall document_render :: Ptr CDocument -> CString -> CInt -> IO CInt
foreign import ccall document_error :: Ptr CDocument -> IO CString
foreign import ccall document_node :: Ptr CDocument -> IO (Ptr CNode)

foreign import ccall node_children :: Ptr CDocument -> Ptr CNode -> Ptr CInt -> Ptr CNode
foreign import ccall node_attributes :: Ptr CDocument -> Ptr CNode -> Ptr CInt -> Ptr CAttr

foreign import ccall node_firstChildBy :: Ptr CDocument -> Ptr CNode -> CString -> CInt -> IO (Ptr CNode)
foreign import ccall node_nextChildBy :: Ptr CDocument -> Ptr CNode -> Ptr CNode -> CString -> CInt -> IO (Ptr CNode)
foreign import ccall node_attributeBy :: Ptr CDocument -> Ptr CNode -> CString -> CInt -> IO (Ptr CAttr)

data Document = Document BS.ByteString (ForeignPtr CDocument)

data Node = Node BS.ByteString (ForeignPtr CDocument) (Ptr CNode)

data Attribute = Attribute BS.ByteString BS.ByteString

parse :: BS.ByteString -> Either BS.ByteString Document
parse src = unsafePerformIO $ BS.unsafeUseAsCStringLen (src <> BS.singleton 0) $ \(str, len) -> do
    doc <- document_parse str (fromIntegral len - 1)
    err <- document_error doc
    if err /= nullPtr then do
        bs <- BS.packCString =<< document_error doc
        document_free doc
        return $ Left bs
     else do
        doc <- newForeignPtr document_free_funptr doc
        return $ Right $ Document src doc

render :: Document -> BS.ByteString
render (Document _ doc) = unsafePerformIO $ withForeignPtr doc $ \d -> do
    i <- document_render d nullPtr 0
    BS.create (fromIntegral i) $ \ptr -> void $ document_render d (castPtr ptr) i


rootNode :: Document -> Node
rootNode d@(Document src doc) = unsafePerformIO $ withForeignPtr doc $ \d -> do
    Node src doc <$> document_node d

applyStr :: BS.ByteString -> Str -> BS.ByteString
applyStr bs Str{..} = BS.take (fromIntegral length) $ BS.drop (fromIntegral start) bs

nodePeek :: Int -> Node -> BS.ByteString
nodePeek i (Node src doc n) = unsafePerformIO $ withForeignPtr doc $ \_ -> do
    applyStr src <$> peekElemOff (castPtr n) i

nodeName :: Node -> BS.ByteString
nodeName = nodePeek 0

nodeInner :: Node -> BS.ByteString
nodeInner = nodePeek 1

nodeOuter :: Node -> BS.ByteString
nodeOuter = nodePeek 2

nodeContents :: Node -> [Either Node BS.ByteString]
nodeContents = undefined

nodeChildren :: Node -> [Node]
nodeChildren = node_children `seq` undefined

nodeAttributes :: Node -> [Attribute]
nodeAttributes = node_attributes `seq` undefined

nodeChildrenBy :: Node -> BS.ByteString -> [Node]
nodeChildrenBy = node_firstChildBy `seq` node_nextChildBy `seq` undefined

nodeAttributeBy :: Node -> BS.ByteString -> Maybe Attribute
nodeAttributeBy = node_attributeBy `seq` undefined
