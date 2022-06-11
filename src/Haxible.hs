-- | The Haxible library entrypoint.
module Haxible where

import Haxible.Codegen
import Haxible.Import
import Haxible.Normalize
import Haxible.Prelude
import Haxible.Syntax
import System.Process.Typed (ExitCode (..), proc, readProcess, runProcess_)

-- | Transform an inventory and playbook into a haskell script.
compile :: FilePath -> FilePath -> IO Text
compile inventory playPath = do
  basePlays <- Haxible.Syntax.decodeFile playPath
  plays <- traverse (Haxible.Import.resolveImport playPath) basePlays
  let exprs = Haxible.Normalize.normalizePlaybook plays
  pure $ renderScript inventory playPath exprs []

-- | Execute the haskell script with cabal.
execute :: FilePath -> IO ()
execute script = runProcess_ (proc "cabal" ["run", "-O0", script])

validate :: FilePath -> IO ()
validate script = do
  (res, stdout, stderr) <- readProcess (proc "runhaskell" [script])
  case res of
    ExitSuccess -> pure ()
    ExitFailure x -> error $ "Process failed with " <> show x <> "\n" <> unsafeFrom stdout <> "\n" <> unsafeFrom stderr
