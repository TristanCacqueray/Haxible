module Main (main) where

import Data.Text.Lazy.Encoding qualified as LText
import Haxible.Codegen qualified
import Haxible.Import
import Haxible.Normalize
import Haxible.Syntax
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Text.Pretty.Simple

main :: IO ()
main = defaultMain (testGroup "Tests" tests)

goldenTest :: Show a => TestName -> IO a -> TestTree
goldenTest name action = goldenVsString name ("test/" <> name <> ".golden") do
  res <- action
  pure . LText.encodeUtf8 . pShowNoColor $ res

tests :: [TestTree]
tests =
  map goldenParse ["demo", "simple", "adder", "loop", "includer"]
  where
    goldenParse name = goldenTest name do
      let playPath = "test/" <> name <> ".yaml"
      basePlays <- Haxible.Syntax.decodeFile playPath
      plays <- traverse (Haxible.Import.resolveImport playPath) basePlays
      let exprs = Haxible.Normalize.normalizePlaybook plays
      pure (exprs, Haxible.Codegen.renderScript "inventory.yaml" exprs)
