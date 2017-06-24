{-# LANGUAGE FlexibleContexts #-}
import Data.Functor.Identity (Identity)
import Text.Parsec
import Test.Hspec
import Fun.Parser
import qualified Fun.Types as Fun

p :: Stream s Identity t => Parsec s () a -> s -> Either ParseError a
p rule = parse rule ""

main :: IO ()
main = hspec $ do
  describe "Fun.Parser.funImport" $ do
    it "parses short form" $
      p funImport "import \"fmt\"" `shouldBe` Right (Fun.Import "fmt" Nothing)

    it "returns error on malformed input" $
      show (p funImport "i_mport \"fmt\"") `shouldBe` "Left (line 1, column 1):\nunexpected \"_\"\nexpecting \"import\""

    it "parses alias" $
      p funImport "import \"longpackagename\" as \"pkg\"" `shouldBe` Right (Fun.Import "longpackagename" (Just "pkg"))

    it "parses nested packages" $
      p funImport "import \"io/ioutil\"" `shouldBe` Right (Fun.Import "io/ioutil" Nothing)

    it "parses github urls" $
      p funImport "import \"github.com/jBugman/fun-lang/fun\"" `shouldBe` Right (Fun.Import "github.com/jBugman/fun-lang/fun" Nothing)

  describe "Fun.Parser.funFuncDecl" $ do
    it "parses simplest decl" $
      p funFuncDecl "f :: IO\n" `shouldBe` Right (Fun.FuncDecl "f" [] Fun.JustIO [] Fun.Undefined)

    it "parses some params" $
      p funFuncDecl "f :: a -> b -> IO\n" `shouldBe`
        Right (Fun.FuncDecl "f" [Fun.Type "a", Fun.Type "b"] Fun.JustIO [] Fun.Undefined)

    it "parses return tuple" $
      p funFuncDecl "f :: a -> b -> (a, b)\n" `shouldBe`
        Right (Fun.FuncDecl "f" [Fun.Type "a", Fun.Type "b"] (Fun.Pure $ Fun.Tuple ["a", "b"]) [] Fun.Undefined)