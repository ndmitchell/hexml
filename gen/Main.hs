
module Main(main) where

import Lib
import Parser
import Control.Monad.Extra
import System.Process.Extra
import System.Directory
import Data.Maybe
import System.IO.Extra
import System.FilePath
import System.Info.Extra


main :: IO ()
main = do
    forM_ examples $ \(b, src) -> do
        putStrLn ""
        putStrLn src
        let (res, trail, err) = run html src
        print res
        print (trail, err)
        putStrLn $ rerender src res
    writeFileChanged "test.h" $ unlines $ compile html
    buildRule ["test.h","test.c"] ["gen" <.> (if isWindows then "exe" else "")] $
        system_ "gcc test.c -o gen -Werror -Wall -Wextra -DTEST"
    system_ "gen"

writeFileChanged :: FilePath -> String -> IO ()
writeFileChanged file new = do
    old <- ifM (doesFileExist file) (readFile' file) (pure "")
    when (old /= new) $
        writeFile file new

buildRule :: [FilePath] -> [FilePath] -> IO () -> IO ()
buildRule from to act = do
    let modTime x = ifM (doesFileExist x) (Just <$> getModificationTime x) (pure Nothing)
    from <- mapM modTime from
    to <- mapM modTime to
    when (any isNothing (from ++ to) || maximum from >= minimum to)
        act

examples :: [(Bool, String)]
examples = take 1
    [(True, "<test foo='123 xyz'><inner /> value</test>")
    ,(True, "<test />")
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
