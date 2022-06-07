module Main (main) where

import Data.List (isSuffixOf)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Haxible.Codegen qualified
import Haxible.Import
import Haxible.Normalize
import Haxible.Prelude
import Haxible.Syntax
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Text.Pretty.Simple

main :: IO ()
main = do
  cases <- filter (isSuffixOf ".yaml") <$> listDirectory "test/playbooks"
  unitsTree <- traverse test cases
  let units = testGroup "Unit" unitsTree
      integrations = testGroup "Integration" []
  defaultMain (testGroup "Tests" [units, integrations])

goldenTest :: Show a => TestName -> a -> TestTree
goldenTest name res = goldenVsString name ("test/playbooks/" <> name) do
  pure . from . Text.encodeUtf8 . Text.dropAround (== '"') . from . pShowNoColor $ res

test :: FilePath -> IO TestTree
test fp = do
  basePlays <- Haxible.Syntax.decodeFile playPath
  plays <- traverse (Haxible.Import.resolveImport playPath) basePlays
  let exprs = Haxible.Normalize.normalizePlaybook plays
      script = Haxible.Codegen.renderScript "inventory.yaml" exprs
  pure $
    testGroup
      name
      [ goldenTest (name <> ".ast") exprs,
        goldenTest (name <> ".hs") script
      ]
  where
    playPath = "test" </> "playbooks" </> fp
    (name, _) = splitExtension fp
