-- | The Haxible library entrypoint.
module Haxible where

import Data.Text.IO qualified (writeFile)
import Haxible.Codegen
import Haxible.Import
import Haxible.Normalize
import Haxible.Prelude
import Haxible.Syntax
import System.Process.Typed (ExitCode (..), proc, readProcess, runProcess_)
import Text.Pretty.Simple (pShowNoColor)

-- | Transform an inventory and playbook into a haskell script.
compile :: FilePath -> FilePath -> IO FilePath
compile inventory playPath = do
  basePlays <- Haxible.Syntax.decodeFile playPath
  plays <- traverse (Haxible.Import.resolveImport playPath) basePlays
  let exprs = Haxible.Normalize.normalizePlaybook plays
  Data.Text.IO.writeFile ast $ from $ pShowNoColor exprs
  Data.Text.IO.writeFile script $ renderScript inventory playPath exprs []
  pure script
  where
    (playbookName, _) = splitExtension playPath
    script = playbookName <> ".hs"
    ast = playbookName <> ".ast"

-- | Execute the haskell script with cabal.
execute :: FilePath -> IO ()
execute script = runProcess_ (proc "cabal" ["run", "-O0", script])

validate :: FilePath -> IO ()
validate script = do
  (res, stdout, stderr) <- readProcess (proc "runhaskell" [script])
  case res of
    ExitSuccess -> pure ()
    ExitFailure x -> error $ "Process failed with " <> show x <> "\n" <> unsafeFrom stdout <> "\n" <> unsafeFrom stderr
