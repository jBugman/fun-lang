module Fun.Tokens
    ( operators
    , keywords
) where

import ClassyPrelude


operators :: [String]
operators =
    [ "="
    , "++"
    , "+"
    , "--"
    , "-"
    , "*"
    , "/"
    , "<"
    , ">"
    , "!"
    , "%"
    , "&&"
    , "||"
    , "&"
    ]

keywords :: [String]
keywords = goKeywords <>
    [ "print"
    , "printf"
    , "set"
    , "slice"
    , "val"
    ]

goKeywords :: [String]
goKeywords =
    [ "break"
    , "case"
    , "chan"
    , "const"
    , "continue"
    , "default"
    , "defer"
    , "else"
    , "fallthrough"
    , "for"
    , "func"
    , "go"
    , "goto"
    , "if"
    , "import"
    , "interface"
    , "map"
    , "package"
    , "range"
    , "return"
    , "select"
    , "struct"
    , "switch"
    , "type"
    , "var"
    ]
