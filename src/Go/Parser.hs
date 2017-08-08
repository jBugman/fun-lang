{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Go.Parser (parse) where

import ClassyPrelude
import Data.Aeson                   (FromJSON, eitherDecodeStrict, parseJSON, withObject, (.:),
                                     (.:?))
import Data.Either.Combinators      (mapLeft)
import Data.SCargot.Repr.WellFormed (pattern L)
import System.Exit                  (ExitCode (..))
import System.IO.Unsafe             (unsafePerformIO)
import System.Process               (readProcessWithExitCode)

import Fun.Errors      (Error (GoError), Pos (..))
import Fun.SExpression (pattern BL, pattern CL, pattern DL, Expression, pattern HL, pattern ID,
                        pattern IL, pattern KW, pattern OL, pattern OP, pattern SL, pattern TP)

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
    parseJSON = withObject "GoError" $ \v -> GoError
        <$> v .:? "pos"
        <*> v .:  "error"

instance FromJSON Expression where
    parseJSON = withObject "Expression" $ \v -> do
        t <- v .: "type"
        case t of
            "List"     -> L <$> v .: "xs"
            "Ident"    -> ID <$> v .: "x"
            "Keyword"  -> KW <$> v .: "x"
            "Operator" -> OP <$> v .: "x"
            "Type"     -> TP <$> v .: "x"
            "String"   -> SL <$> v .: "x"
            "Char"     -> CL <$> v .: "x"
            "Integer"  -> IL <$> v .: "x"
            "Double"   -> DL <$> v .: "x"
            "Oct"      -> OL <$> v .: "x"
            "Hex"      -> HL <$> v .: "x"
            "Bool"     -> BL <$> v .: "x"
            _          -> fail $ "not a valid type: " <> t
