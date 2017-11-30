{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib(
    Parser,
    out, many, optional, match, lit, choice, abort, choices,
    run,
    compile
    ) where

import Control.Monad.Extra
import Control.Monad.Trans.Writer
import Data.Char
import Data.Maybe
import Data.List
import Data.Data
import Data.Bits
import Data.Monoid
import Numeric


run :: Parser o () -> String -> ([(Int, o)], (Int, String), Maybe String)
run p = runC (toC $ toP p)

compile :: Show o => Parser o () -> [String]
compile = prettyC . optimiseC . toC . toP

---------------------------------------------------------------------
-- THE UNDERLYING PARSER TYPE

data P o
    = Empty
    | Seq (P o) (P o)
    | Choice (P o) (P o)
    | Match CharSet
    | Many (P o)
    | Out o
    | Panic String
      deriving Show

instance Monoid (P o) where
    mempty = Empty
    mappend = Seq

newtype Parser o a = Parser (Writer (P o) a)
    deriving (Functor, Applicative, Monad)

toP :: Parser o a -> P o
toP (Parser p) = execWriter p

out :: o -> Parser o ()
out o = Parser $ tell $ Out o

abort :: String -> Parser o ()
abort x = Parser $ tell $ Panic x

many :: Parser o () -> Parser o ()
many x = Parser $ tell $ Many $ toP x

match :: (Char -> Bool) -> Parser o ()
match f = Parser $ tell $ Match $ CharSet f

lit :: String -> Parser o ()
lit xs = forM_ xs $ \x -> match (== x)

choice :: Parser o () -> Parser o () -> Parser o ()
choice a b = Parser $ tell $ Choice (toP a) (toP b)

choices :: [Parser o ()] -> Parser o ()
choices = foldr1 choice

optional :: Parser o () -> Parser o ()
optional x = choice x $ return ()


---------------------------------------------------------------------
-- CHARSET

chars = ['\1'..'\xff']

newtype CharSet = CharSet {fromCharSet :: Char -> Bool}

instance Eq CharSet where
    CharSet f1 == CharSet f2 = all (\x -> f1 x == f2 x) chars

instance Monoid CharSet where
    mempty = CharSet $ const False
    mappend (CharSet f1) (CharSet f2) = CharSet $ f1 ||^ f2

instance Show CharSet where
    show (CharSet p) = "[" ++ f chars ++ "]"
        where
            showChar x | x `elem` "[]-" = ['\\',x]
                       | otherwise = init $ tail $ show x
            f xs = case dropWhile (not . p) xs of
                [] -> []
                x:xs -> case span p xs of
                    ([], rest) -> showChar x ++ f rest :: String
                    (ys, rest) -> showChar x ++ "-" ++ showChar (last ys) ++ f rest

-- | Given we know the first argument are all true, can the second argument ever be False
implies :: [CharSet] -> CharSet -> Bool
implies known x = all (fromCharSet x) poss
    where poss = filter (\c -> all (\k -> fromCharSet k c) known) chars

charSetC :: [CharSet] -> ([String], CharSet -> String)
charSetC sets =
    (["const char parse_table[256] = {" ++ intercalate "," ("0":map disp chars) ++ "};"]
    ,\x -> fromMaybe ("parse_table[*p] & " ++ showBit (bit $ fromJust $ elemIndex x hard)) $ simple x)
    where
        showBit x = "0x" ++ showHex (x :: Int) ""
        disp c = showBit $ foldr (.|.) 0 [bit i | (i, h) <- zip [0..] hard, fromCharSet h c]

        hard = nub $ filter (isNothing . simple) sets

        simple x | x == CharSet (const True) = Just "*p"
                 | [x] <- filter (fromCharSet x) chars = Just $ "*p == " ++ show x
                 | otherwise = Nothing

---------------------------------------------------------------------
-- THE UNDERLYING C TYPE

data C o
    = While CharSet [C o]
    | Stmt o
    | Next
    | If CharSet [C o] [C o]
    | Abort String
      deriving Show

-- Can you leave this statement successfully having consumed no characters?
empty :: P o -> Bool
empty Empty = True
empty (Choice a b) = empty b
empty Many{} = True
empty Out{} = True
empty Match{} = False
empty Panic{} = False
empty (Seq a b) = empty a && empty b

-- What characters can come next and you commit
peek :: P o -> CharSet
peek Empty = mempty
peek Out{} = mempty
peek (Match f) = f
peek Panic{} = CharSet $ const True
peek (Choice a b) = peek a <> peek b
peek (Many x) = peek x
peek (Seq a b) = peek a <> (if empty a then peek b else mempty)

toC :: P o -> [C o]
toC Empty = []
toC (Choice a b) = [If (peek a) (toC a) (toC b)]
toC (Match f) = [If f [Next] [Abort "Failed to match"]]
toC (Many x) = [While (peek x) (toC x)]
toC (Out x) = [Stmt x]
toC (Panic x) = [Abort x]
toC (Seq a b) = toC a ++ toC b

runC :: [C o] -> String -> ([(Int, o)], (Int, String), Maybe String)
runC = f 0
    where
        f i [] s = ([], (i, s), Nothing)
        f i (Abort x:_) s = ([], (i,s), Just x)
        f i (Next:_) [] = ([], (i, []), Just "Next off the end")
        f i (Next:r) (x:xs) = f (i+1) r xs
        f i (Stmt x:r) xs = let (v1,v2,v3) = f i r xs in ((i,x):v1, v2, v3)
        f i (If cond true false : r) xs
            | x:_ <- xs, fromCharSet cond x = f i (true ++ r) xs
            | otherwise = f i (false ++ r) xs
        f i (While cond x:r) xs = f i (If cond (x ++ [While cond x]) [] : r) xs


prettyC :: Show o => [C o] -> [String]
prettyC xs =
    prefix ++
    ["const char* parser(const char* p ARGUMENTS){"] ++
    fi xs ++
    ["  return NULL;"
    ,"}"]
    where
        (prefix, cond) = charSetC $ concatMap charSets xs
        charSets (While c x) = c : concatMap charSets x
        charSets (If c t f) = c : concatMap charSets t ++ concatMap charSets f
        charSets _ = []

        fi = map ("  "++) . concatMap f
        f (Abort x) = ["abort(" ++ show x ++ ");"]
        f Next = ["p++;"]
        f (Stmt x) = [show x ++ ";"]
        f (If c true []) = ["if (" ++ cond c ++ "){"] ++ fi true ++ ["}"]
        f (If c true [false@If{}]) = ["if (" ++ cond c ++ "){"] ++ fi true ++ ["} else " ++ f1] ++ fs
            where f1:fs = f false
        f (If c true false) = ["if (" ++ cond c ++ "){"] ++ fi true ++ ["} else {"] ++ fi false ++ ["}"]
        f (While c x) = ["while (" ++ cond c ++ "){"] ++ fi x ++ ["}"]

optimiseC :: [C o] -> [C o]
optimiseC = f []
    where
        f known [] = []
        f known (Abort x:_) = [Abort x]
        f known (Next:xs) = Next : f [] xs
        f known (Stmt x:xs) = Stmt x : f known xs
        f known (If cond true false : xs)
            | implies known cond = f known $ true ++ xs
            | otherwise = If cond (f (cond:known) true) (f known false) : f [] xs -- might be a Next in body
        f known (While cond x : xs) = While cond (f (cond:known) x) : f [] xs
