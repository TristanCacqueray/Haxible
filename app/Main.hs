-- | This module contains the command line entrypoint
module Main (main) where

import Control.Monad (unless)
import Data.Text.IO qualified (writeFile)
import Haxible (compile, execute)
import Options.Generic

data CLI w = CLI
  { playbook :: w ::: FilePath <?> "YAML file to interpret",
    inventory :: w ::: FilePath <?> "Inventory path",
    dry :: w ::: Bool <?> "Don't run the playbook, just compile it"
  }
  deriving (Generic)

main :: IO ()
main = do
  cli <- parseArgs
  code <- compile cli.inventory cli.playbook
  let script = cli.playbook <> ".hs"
  Data.Text.IO.writeFile script code
  unless cli.dry $ execute script

instance ParseRecord (CLI Wrapped)

deriving instance Show (CLI Unwrapped)

parseArgs :: IO (CLI Unwrapped)
parseArgs = unwrapRecord "Haxible - Ansible interpreter powered by Haxl"
