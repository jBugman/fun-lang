module Test.Examples where

import ClassyPrelude
import Test.Hspec    (Spec, describe, it)

import Fun        (translate)
import Test.Utils (shouldPrint)


examplesSpec :: Spec
examplesSpec =

    it "01_hello_world" $
      translate "(package main\n\n(func main (print \"hello world\")))\n"
      `shouldPrint`
      "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"hello world\")\n}\n"

