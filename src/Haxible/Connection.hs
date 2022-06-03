-- | This module contains the logic to interact with the python wrapper
module Haxible.Connection (Connections (..), TaskCall (..), withConnections) where

import Control.Exception (bracket)
import Data.Aeson (Value (Object), eitherDecodeStrict, encode)
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified
import Data.Bifunctor (first)
import Data.ByteString (hGetLine, toStrict)
import Data.ByteString.Char8 (hPutStrLn)
import Data.Hashable (Hashable)
import Data.Pool qualified
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.IO.Handle (hFlush)
import Say
import System.IO (Handle, hClose)
import System.Process.Typed

data TaskCall = TaskCall
  { -- | The playbook attributes, such as `hosts` or `become`
    playAttrs :: [(Text, Value)],
    -- | The task object, e.g `{"file": {"path": "/etc/zuul"}}`
    taskObject :: Value,
    -- | A list of variables, such as `register` names or `include_vars`
    env :: [(Text, Value)]
  }
  deriving (Eq, Show, Typeable, Generic, Hashable)

-- | A connection run converts a TaskCall into a (result code, result value)
newtype Connections = Connections {run :: TaskCall -> IO (Int, Value)}

-- | Creates the Python interpreters.
withConnections :: Int -> FilePath -> (Connections -> IO ()) -> IO ()
withConnections count inventory callback =
  bracket (Data.Pool.newPool poolConfig) Data.Pool.destroyAllResources go
  where
    go pool = do
      putStrLn "Pool ready"

      let runTask :: TaskCall -> Process Handle Handle () -> IO (Int, Value)
          runTask taskCall p = do
            let callParams = [mkObj taskCall.playAttrs, taskCall.taskObject, mkObj taskCall.env]
            say $ " ▶ Calling " <> Text.pack (show taskCall)
            hPutStrLn (getStdin p) (toStrict $ encode callParams)
            hFlush (getStdin p)
            output <- hGetLine (getStdout p)
            case eitherDecodeStrict output of
              Right res -> pure res
              Left err -> error $ show output <> ": " <> err

      callback (Connections $ Data.Pool.withResource pool . runTask)

    mkObj = Object . Data.Aeson.KeyMap.fromList . map (first Data.Aeson.Key.fromText)

    poolConfig :: Data.Pool.PoolConfig (Process Handle Handle ())
    poolConfig =
      Data.Pool.PoolConfig
        { createResource = do
            startProcess
              . setStdin createPipe
              . setStdout createPipe
              $ proc "python" ["./app/wrapper.py", inventory],
          freeResource = \p -> do
            hClose (getStdin p)
            stopProcess p,
          poolCacheTTL = 3600,
          poolMaxResources = count
        }
