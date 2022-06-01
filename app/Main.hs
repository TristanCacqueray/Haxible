-- | This module contains the command line entrypoint
module Main (main) where

import Control.Monad (unless)
import Data.Text.IO qualified (writeFile)
import Haxible.Parser (decodePlaybook, renderScript)
import Options.Generic
import System.Process.Typed (proc, runProcess_)

data CLI w = CLI
  { playbook :: w ::: FilePath <?> "YAML file to interpret",
    dry :: w ::: Bool <?> "Don't run the playbook, just compile it"
  }
  deriving (Generic)

main :: IO ()
main = do
  -- uasge
  cli <- parseArgs

  -- parse
  pb <- decodePlaybook cli.playbook

  -- render
  let script = cli.playbook <> ".hs"
  Data.Text.IO.writeFile script (renderScript pb)

  -- execute
  unless cli.dry do
    runProcess_ (proc "cabal" ["run", script])

instance ParseRecord (CLI Wrapped)

deriving instance Show (CLI Unwrapped)

parseArgs :: IO (CLI Unwrapped)
parseArgs = unwrapRecord "Haxible - Ansible interpreter powered by Haxl"
