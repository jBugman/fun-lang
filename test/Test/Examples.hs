module Test.Examples (examples) where

import ClassyPrelude              hiding (readFile)
import System.FilePath            ((<.>), (</>))
import Test.Tasty                 (TestName, TestTree, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)

import Fun (translate, unError)


examples :: TestTree
examples = testGroup "Examples"
    [ translateExample "01_hello_world"
    , translateExample "02_values"
    ]


translateExample :: TestName -> TestTree
translateExample name = goldenTest
    name
    (readGolden name)
    (translated name)
    cmpFunc
    updateFunc

readFile :: FilePath -> String -> IO Text
readFile name ext = readFileUtf8 ("examples" </> name <.> ext)

readGolden :: FilePath -> IO Text
readGolden path = readFile path ".go"

translated :: FilePath -> IO Text
translated name = do
    src <- readFile name ".fun"
    return $ either unError id $ translate src

cmpFunc :: Text -> Text -> IO (Maybe String)
cmpFunc expected actual = pure $ if expected == actual
    then Nothing
    else Just . unpack $ "Test output was different from golden file. It was: " <> actual

updateFunc :: Text -> IO ()
updateFunc _ = pure ()
