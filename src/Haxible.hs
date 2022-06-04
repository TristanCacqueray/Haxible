-- | The Haxible library entrypoint.
module Haxible where

import Haxible.Codegen
import Haxible.Import
import Haxible.Normalize
import Haxible.Prelude
import Haxible.Syntax
import System.Process.Typed (proc, runProcess_)

-- | Transform an inventory and playbook into a haskell script.
compile :: FilePath -> FilePath -> IO Text
compile inventory playPath = do
  basePlays <- Haxible.Syntax.decodeFile playPath
  plays <- traverse (Haxible.Import.resolveImport playPath) basePlays
  let exprs = Haxible.Normalize.normalizePlaybook plays
  pure $ renderScript inventory exprs

-- | Execute the haskell script with cabal.
execute :: FilePath -> IO ()
execute script = runProcess_ (proc "cabal" ["run", "-O0", script])
