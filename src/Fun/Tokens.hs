module Fun.Tokens where

import ClassyPrelude (String)


operators :: [String]
operators =
    [ "="
    , "+"
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
keywords =
    [ "func"
    , "package"
    , "import"
    , "for"
    , "print"
    , "set"
    , "var"
    , "const"
    ]
