
module Main(main) where

import Lib
import Parser
import Control.Monad


main :: IO ()
main = do
    writeFile "test.h" $ unlines $ compile html
    forM_ examples $ \(b, src) -> do
        putStrLn ""
        putStrLn src
        let (res, trail, err) = run html src
        print res
        print (trail, err)
        putStrLn $ rerender src res


examples :: [(Bool, String)]
examples =
    [(True, "<test />")
    ,(True, "<test id='bob'>here<extra/>there</test>")
    ,(True, "<test /><close />")
    ,(True, "<test /><!-- comment > --><close />")
    ,(True, "<test id=\"bob value\" another-attr=\"test with <\">here </test> more text at the end<close />")
    ,(False, "<test></more>")
    ,(False, "<test")
    ,(True, "<?xml version=\"1.1\"?>\n<greeting>Hello, world!</greeting>")
    ]

rerender :: String -> [(Int, Out)] -> String
rerender o = f
    where
        sub s e = take (e - s) $ drop s o

        f ((_,Tag):(_,TagComment):xs) = f xs
        f ((_,Tag):xs) = "<" ++ f xs
        f ((_,TagOpen):xs) = ">" ++ f xs
        f ((_,TagOpenClose):xs) = "/>" ++ f xs
        f ((s,NameStart):(e,NameEnd):(_,TagClose):xs) = "/" ++ sub s e ++ ">" ++ f xs
        f ((s,NameStart):(e,NameEnd):xs) = sub s e ++ " " ++ f xs
        f ((s,QuoteStart):(e,QuoteEnd):xs) = "=\"" ++ sub s e ++ "\" " ++ f xs
        f ((_,AttribsStart):xs) = f xs
        f ((_,AttribsEnd):xs) = f xs
        f [] = []
