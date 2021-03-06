{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foreign.Parser (parse) where

import ClassyPrelude
import Data.Aeson              (FromJSON, Object, eitherDecodeStrict, parseJSON, withObject, (.!=),
                                (.:), (.:?))
import Data.Aeson.Types        (Parser)
import Data.Either.Combinators (mapLeft)
import System.Exit             (ExitCode (..))
import System.IO.Unsafe        (unsafePerformIO)
import System.Process          (readProcessWithExitCode)

import Fun.Errors      (Error (..), Pos (..))
import Fun.SExpression (Atom (..), Expression (..), pattern INT, pattern KW, pattern L,
                        Literal (..))

parse :: Text -> Either Error Expression
parse src = case goParse src of
    Right json -> unmarshalExpr json
    Left  json -> Left . unmarshalError $ json

{-# NOINLINE goParse #-}
goParse :: Text -> Either Text Text
goParse src = unsafePerformIO $ do
    (exitcode, output, errors) <- readProcessWithExitCode "func-parse" [] (unpack src)
    return $ case exitcode of
        ExitSuccess   -> Right $ pack output
        ExitFailure _ -> Left  $ pack errors

unmarshalExpr :: Text -> Either Error Expression
unmarshalExpr t = mapLeft wrapErr . eitherDecodeStrict $ encodeUtf8 t

unmarshalError :: Text -> Error
unmarshalError t = either wrapErr id . eitherDecodeStrict $ encodeUtf8 t

wrapErr :: String -> Error
wrapErr e = GoError Nothing $ pack e

instance FromJSON Pos where
    parseJSON = withObject "Pos" $ \v -> Pos
        <$> v .: "ln"
        <*> v .: "col"

instance FromJSON Error where
    parseJSON = withObject "SyntaxError" $ \v -> SyntaxError
        <$> v .:? "pos"
        <*> v .:  "error"

instance FromJSON Expression where
    parseJSON = withObject "Expression" $ \v -> do
        t <- v .: "type"
        case t of
            "List"     -> L   <$> v .:? "xs" .!= []
            "Ident"    -> mkAtom v Ident
            "Keyword"  -> KW  <$> v .:  "x"
            "Operator" -> mkAtom v Operator
            "Type"     -> mkAtom v Type
            "String"   -> mkAtom v (Literal . String)
            "Char"     -> mkAtom v (Literal . Char)
            "Integer"  -> INT <$> v .:? "base" .!= 10 <*> v .: "x" <*> v .: "pos"
            "Double"   -> mkAtom v (Literal . Double)
            "Bool"     -> mkAtom v (Literal . Bool)
            _          -> fail $ "not a valid type: " <> t

mkAtom :: FromJSON a => Object -> (a -> Atom) -> Parser Expression
mkAtom o a = Atom <$> (a <$> o .: "x" ) <*> o .: "pos"
