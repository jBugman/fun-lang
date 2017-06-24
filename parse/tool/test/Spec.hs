{-# LANGUAGE FlexibleContexts #-}
import Data.Functor.Identity (Identity)
import Text.Parsec
import Test.Hspec
import Fun.Parser
import Fun.Types

p :: Stream s Identity t => Parsec s () a -> s -> Either ParseError a
p rule = parse rule ""

main :: IO ()
main = hspec $
  describe "Fun.Parser.funImport" $ do
    it "parses short form" $
      p funImport "import \"fmt\"" `shouldBe` Right (Import "fmt" Nothing)

    it "returns error on malformed input" $
      show (p funImport "i_mport \"fmt\"") `shouldBe` "Left (line 1, column 1):\nunexpected \"_\"\nexpecting \"import\""

    it "parses alias" $
      p funImport "import \"longpackagename\" as \"pkg\"" `shouldBe` Right (Import "longpackagename" (Just "pkg"))

    it "parses nested packages" $
      p funImport "import \"io/ioutil\"" `shouldBe` Right (Import "io/ioutil" Nothing)

    it "parses github urls" $
      p funImport "import \"github.com/jBugman/fun-lang/fun\"" `shouldBe` Right (Import "github.com/jBugman/fun-lang/fun" Nothing)