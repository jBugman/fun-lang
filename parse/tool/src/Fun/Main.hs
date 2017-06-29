{-# LANGUAGE PartialTypeSignatures #-}
module Fun.Main
    ( parsePackage
    , printAsJSON
    , prettyError
    ) where

import Prelude hiding (putStr, exp)
import Data.Aeson (encode, ToJSON)
import Data.ByteString.Lazy (ByteString, putStr)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.List.NonEmpty as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Error as PE

import Fun.Types (Package)
import Fun.Parser (prs, package)


parsePackage :: String -> Either (P.ParseError Char _) Package
parsePackage = prs package

jsonify :: ToJSON t => t -> ByteString
jsonify = encode

printAsJSON :: ToJSON t => t -> IO ()
printAsJSON = putStr . jsonify

prettyError :: P.ShowErrorComponent e => P.ParseError Char e -> String
prettyError err =
        msg ++ "at Ln " ++ show line ++ ", Col " ++ show col
        where
            pos = L.head (P.errorPos err)
            line = P.unPos $ P.sourceLine pos
            col = P.unPos $ P.sourceColumn pos
            msg = replace "\n" " "
                $ replace "unexpected" "found"
                $ replace "expecting" "expected"
                $ PE.parseErrorTextPretty err

            replace :: Eq a => [a] -> [a] -> [a] -> [a]
            replace old new xs = intercalate new . splitOn old $ xs
