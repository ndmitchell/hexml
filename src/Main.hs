
module Main(main) where

import Text.XML.Hexml
import qualified Data.ByteString as BS

main :: IO ()
main = do
    bs <- BS.readFile "C:/Neil/notes/core.xml"
    case parse bs of
        Left err -> error $ show err
        Right val -> print $ render val
