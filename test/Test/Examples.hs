module Test.Examples where

import ClassyPrelude
import Test.Hspec    (Spec, it, runIO)

import Fun        (translate)
import Test.Utils (shouldPrint)


examplesSpec :: Spec
examplesSpec =

    it "01_hello_world" $ translate (unlines

      [ "(package main"
      , ""
      , "(func main (print \"hello world\")))"
      ]
      ) `shouldPrint` unlines

      [ "package main"
      , ""
      , "import \"fmt\""
      , ""
      , "func main() {"
      , "\tfmt.Println(\"hello world\")"
      , "}"
      ]


readExample :: Text -> IO Text
readExample name = readFileUtf8 $ unpack ("examples/" <> name <> ".fun")
