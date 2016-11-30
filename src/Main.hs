{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Text.XML.Hexml
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Data.Monoid
import Data.Char


examples :: [(Bool, BS.ByteString)]
examples =
    [(True, "<test id='bob'>here<extra/>there</test>")
    ,(True, "<test /><close />")
    ,(True, "<test id=\"bob value\" another-attr=\"test with <\">here </test> more text at the end<close />")
    ,(False, "<test></more>")
    ,(False, "<test")
    ,(True, "<?xml version=\"1.1\"?>\n<greeting>Hello, world!</greeting>")
    ]

main :: IO ()
main = do
    forM_ examples $ \(parses, src) -> do
        print src
        case nodeParse src of
            Left err -> when parses $ fail $ "Unexpected parse failure, " ++ show err
            Right doc -> do
                unless parses $ fail "Unexpected parse success"
                let r = nodeRender doc
                print r
                print $ rerender doc
                when (r /= rerender doc) $ fail "Different rerender"
                let Right d = nodeParse r
                when (r /= nodeRender d) $ fail "Different after rerendering"

    let Right doc = nodeParse "<test id=\"1\" extra=\"2\" /><test id=\"2\" /><b><test id=\"3\" /></b><test id=\"4\" /><test />"
    map nodeName (nodeChildren doc) === ["test","test","b","test","test"]
    length (nodeChildrenBy doc "test") === 4
    length (nodeChildrenBy doc "b") === 1
    length (nodeChildrenBy doc "extra") === 0
    nodeAttributes (head $ nodeChildren doc) === [Attribute "id" "1", Attribute "extra" "2"]
    map (`nodeAttributeBy` "id") (nodeChildrenBy doc "test") === map (fmap (Attribute "id")) [Just "1", Just "2", Just "4", Nothing]


a === b = if a == b then putStrLn "success" else fail "mismatch"

rerender :: Node -> BS.ByteString
rerender = contents
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
