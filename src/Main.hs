{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Text.XML.Hexml
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Data.Monoid
import Data.Char


examples :: [(Bool, BS.ByteString)]
examples =
    [(True, "<test id=bob>here<extra/>there</test>")
    ,(True, "<test /><close />")
    ,(True, "<test id=\"bob value\" another-attr=\"test with <\">here </test> more text at the end<close />")
    ,(False, "<test></more>")
    ,(False, "<test")
    ]

main :: IO ()
main = do
    forM_ examples $ \(parses, src) -> do
        print src
        case documentParse src of
            Left err -> when parses $ fail $ "Unexpected parse failure, " ++ show err
            Right doc -> do
                unless parses $ fail "Unexpected parse success"
                let r = documentRender doc
                print r
                print $ rerender doc
                when (r /= rerender doc) $ fail "Different rerender"
                let Right d = documentParse r
                when (r /= documentRender d) $ fail "Different after rerendering"


rerender :: Document -> BS.ByteString
rerender = contents . documentNode
    where
        contents x = BS.concat $ map (either validStr node) $ nodeContents x
        node x = "<" <> BS.unwords (validName (nodeName x) : map attr (nodeAttributes x)) <> ">" <>
                 contents x <>
                 "</" <> nodeName x <> ">"
        attr (Attribute a b) = validName a <> "=\"" <> validAttr b <> "\""

        validName x | BS.all (\x -> isAlphaNum x || x `elem` ("-:_" :: String)) x = x
                    | otherwise = error "Invalid name"
        validAttr x | BS.notElem '\"' x = x
                    | otherwise = error "Invalid attribute"
        validStr x | BS.notElem '<' x = x
                   | otherwise = error $ show ("Invalid string", x)
