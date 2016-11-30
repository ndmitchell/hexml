{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Text.XML.Hexml as X
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Data.Monoid
import Data.Char


examples :: [(Bool, BS.ByteString)]
examples =
    [(True, "<test id='bob'>here<extra/>there</test>")
    ,(True, "<test /><close />")
    ,(True, "<test /><!-- comment > --><close />")
    ,(True, "<test id=\"bob value\" another-attr=\"test with <\">here </test> more text at the end<close />")
    ,(False, "<test></more>")
    ,(False, "<test")
    ,(True, "<?xml version=\"1.1\"?>\n<greeting>Hello, world!</greeting>")
    ]

main :: IO ()
main = do
    forM_ examples $ \(parses, src) -> do
        print src
        case parse src of
            Left err -> when parses $ fail $ "Unexpected parse failure, " ++ show err
            Right doc -> do
                unless parses $ fail "Unexpected parse success"
                let r = render doc
                print r
                print $ rerender doc
                when (r /= rerender doc) $ fail "Different rerender"
                let Right d = parse r
                when (r /= render d) $ fail "Different after rerendering"

    let Right doc = parse "<test id=\"1\" extra=\"2\" /><test id=\"2\" /><b><test id=\"3\" /></b><test id=\"4\" /><test />"
    map name (children doc) === ["test","test","b","test","test"]
    length (childrenBy doc "test") === 4
    length (childrenBy doc "b") === 1
    length (childrenBy doc "extra") === 0
    attributes (head $ children doc) === [Attribute "id" "1", Attribute "extra" "2"]
    map (`attributeBy` "id") (childrenBy doc "test") === map (fmap (Attribute "id")) [Just "1", Just "2", Just "4", Nothing]

    Right _ <- return $ parse $ "<test " <> BS.unwords [BS.pack $ "x" ++ show i ++ "='value'" | i <- [1..10000]] <> " />"
    Right _ <- return $ parse $ BS.unlines $ replicate 10000 "<test x='value' />"

    let attrs = ["usd:jpy","test","extra","more","stuff","jpy:usd","xxx","xxxx"]
    Right doc <- return $ parse $ "<test " <> BS.unwords [x <> "='" <> x <> "'" | x <- attrs] <> ">middle</test>"
    [c] <- return $ childrenBy doc "test"
    forM_ attrs $ \a -> attributeBy c a === Just (Attribute a a)
    forM_ ["missing","gone","nothing"] $ \a -> attributeBy c a === Nothing
    putStrLn "Done"


a === b = if a == b then putStrLn "success" else fail "mismatch"

rerender :: Node -> BS.ByteString
rerender = inside
    where
        inside x = BS.concat $ map (either validStr node) $ contents x
        node x = "<" <> BS.unwords (validName (name x) : map attr (attributes x)) <> ">" <>
                 inside x <>
                 "</" <> name x <> ">"
        attr (Attribute a b) = validName a <> "=\"" <> validAttr b <> "\""

        validName x | BS.all (\x -> isAlphaNum x || x `elem` ("-:_" :: String)) x = x
                    | otherwise = error "Invalid name"
        validAttr x | BS.notElem '\"' x = x
                    | otherwise = error "Invalid attribute"
        validStr x | BS.notElem '<' x || BS.isInfixOf "<!--" x = x
                   | otherwise = error $ show ("Invalid string", x)
