module Main (main) where

import Data.Text.Lazy.Encoding qualified as LText
import Haxible.Parser qualified
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
  [goldenParse "demo", goldenParse "simple"]
  where
    goldenParse name = goldenTest name do
      pb <- Haxible.Parser.decodePlaybook ("test/" <> name <> ".yaml")
      pure (pb, Haxible.Parser.renderScript "inventory.yaml" pb)
