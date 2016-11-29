
module Text.XML.Hexml(
    Document, Node, Attribute(..),
    parse, render, rootNode,
    nodeInner, nodeOuter, nodeContents, nodeChildren, nodeAttributes,
    nodeChildrenBy, nodeAttributeBy
    ) where

import Control.Monad
import Data.Int
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.Unsafe
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Internal as BS

data CDocument
data CNode
data CAttr

data Str = Str {start :: {-# UNPACK #-} !Int32, length :: {-# UNPACK #-} !Int32}

foreign import ccall document_parse :: CString -> CInt -> IO (Ptr CDocument)
foreign import ccall document_free :: Ptr CDocument -> IO ()
foreign import ccall "&document_free" document_free_funptr :: FunPtr (Ptr CDocument -> IO ())
foreign import ccall document_render :: Ptr CDocument -> CString -> CInt -> IO CInt
foreign import ccall document_error :: Ptr CDocument -> IO CString
foreign import ccall document_node :: Ptr CDocument -> IO (Ptr CNode)

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

nodeInner :: Node -> BS.ByteString
nodeInner (Node src doc i) = undefined

nodeOuter :: Node -> BS.ByteString
nodeOuter = undefined

nodeContents :: Node -> [Either Node BS.ByteString]
nodeContents = undefined

nodeChildren :: Node -> [Node]
nodeChildren = undefined

nodeAttributes :: Node -> [Attribute]
nodeAttributes = undefined

nodeChildrenBy :: Node -> BS.ByteString -> [Node]
nodeChildrenBy = undefined

nodeAttributeBy :: Node -> BS.ByteString -> Maybe Attribute
nodeAttributeBy = undefined
