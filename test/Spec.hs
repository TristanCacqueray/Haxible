module Main (main) where

import Data.List (isSuffixOf)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Haxible
import Haxible.Codegen qualified
import Haxible.Import
import Haxible.Normalize
import Haxible.Prelude
import Haxible.Syntax
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase)
import Text.Pretty.Simple

main :: IO ()
main = do
  cases <- map dropExtension . filter (isSuffixOf ".yaml") <$> listDirectory "test/playbooks"
  (expectedNames, unitsTree) <- unzip <$> traverse test cases
  integrationTree <- do
    ansibleExist <- findExecutable "ansible-playbook"
    case ansibleExist of
      Just _ -> pure $ integration <$> (catMaybes expectedNames)
      Nothing -> pure []
  let units = testGroup "Unit" unitsTree
      integrations = testGroup "Integration" integrationTree
  defaultMain (testGroup "Tests" [units, after AllSucceed "Unit" integrations])

goldenTest :: Show a => TestName -> a -> TestTree
goldenTest name res = goldenVsString name ("test/playbooks/" <> name) do
  pure . from . Text.encodeUtf8 . Text.dropAround (== '"') . from . pShowNoColor $ res

getExpected :: String -> IO [Value]
getExpected name = do
  exist <- doesFileExist expectedPath
  if exist then Haxible.Syntax.decodeFile expectedPath else pure []
  where
    expectedPath = "test/playbooks/" <> name <> ".expect"

integration :: String -> TestTree
integration name = do
  testCase name (Haxible.validate scriptPath)
  where
    scriptPath = "test/playbooks/" <> name <> ".hs"

test :: String -> IO (Maybe String, TestTree)
test name = do
  basePlays <- Haxible.Syntax.decodeFile playPath
  plays <- traverse (Haxible.Import.resolveImport playPath) basePlays
  expected <- getExpected name
  let exprs = Haxible.Normalize.normalizePlaybook plays
      script = Haxible.Codegen.renderScript "inventory.yaml" playPath exprs expected
  pure $
    ( if null expected then Nothing else Just name,
      testGroup
        name
        [ goldenTest (name <> ".ast") exprs,
          goldenTest (name <> ".hs") script
        ]
    )
  where
    playPath = "test" </> "playbooks" </> name <> ".yaml"
