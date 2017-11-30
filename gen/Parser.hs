
module Parser(Out(..), html) where

import Lib

data Out
    = NameStart | NameEnd
    | QuoteStart | QuoteEnd
    | AttribsStart | AttribsEnd
    | Tag
    | TagComment | TagOpen | TagClose | TagOpenClose
      deriving Show

is lower upper x = x >= lower && x <= upper
isName1 x = x == ':' || x == '_' || is 'a' 'z' x || is 'A' 'Z' x
isName x = isName1 x || x == '-' || is '0' '9' x
isSpace x = x == ' ' || x == '\t' || x == '\r' || x == '\n'

whitespace = many $ match isSpace

name = do
    out NameStart
    match isName1
    many $ match isName
    whitespace
    out NameEnd

attrib = do
    name
    lit "=" `choice` abort "Expected = in attribute but missing"
    whitespace
    let quoted x = do
            lit [x]
            out QuoteStart
            many $ match (/= x)
            out QuoteEnd
            lit [x] `choice` abort ("Couldn't find closing attribute quote (expected " ++ [x] ++ ")")
            whitespace
    choices
        [quoted '\"'
        ,quoted '\''
        ,abort "Invalid attribute, expected quote (either ' or \")"]

attribs = do
    out AttribsStart
    many attrib
    out AttribsEnd

tag = do
    out Tag
    lit "<"
    let name_ = name `choice` abort "Missing tag name"
    choices
        [do lit "!--"
            many $ lit "-->" -- FIXME: Not correct!!!
            out TagComment
        ,do lit "?"
            name_
            attribs
            lit "?>"
            out TagOpenClose
        ,do lit "/"
            name_
            lit ">"
            out TagClose
        ,do name_
            attribs
            choice
                (lit "/>" >> out TagOpenClose)
                (lit ">" >> out TagOpen)
        ]


html = many $ choice tag $ match $ const True
