-- | This module contains the logic to interact with the python interpreter
module Haxible.Connection (Connections (..), withConnections) where

import Data.Aeson (Value (String), eitherDecodeStrict, encode)
import Data.ByteString (hGetLine, toStrict)
import Data.ByteString.Char8 (hPutStrLn)
import Data.Pool qualified
import Data.Text (Text)
import GHC.IO.Handle (hFlush)
import System.IO (Handle, hClose)
import System.Process.Typed

-- | Python calls takes a JSON value and produces a JSON value.
newtype Connections = Connections {call :: [(Text, Value)] -> IO [Value]}

-- | Creates the Python interpreter.
withConnections :: Int -> (Connections -> IO ()) -> IO ()
withConnections count callback = do
  pool <- Data.Pool.newPool poolConfig
  go pool
  where
    go pool = do
      putStrLn "Pool ready"

      let runTask :: (Text, Value) -> Process Handle Handle () -> IO Value
          runTask (task, attr) p = do
            hPutStrLn (getStdin p) (toStrict $ encode [String task, attr])
            hFlush (getStdin p)
            output <- hGetLine (getStdout p)
            case eitherDecodeStrict output of
              Right res -> pure res
              Left err -> error $ show output <> ": " <> err

      let cb :: [(Text, Value)] -> IO [Value]
          cb tasks = do
            traverse (\x -> Data.Pool.withResource pool (runTask x)) tasks

      callback (Connections cb)

    poolConfig :: Data.Pool.PoolConfig (Process Handle Handle ())
    poolConfig =
      Data.Pool.PoolConfig
        { createResource = do
            startProcess (setStdin createPipe $ setStdout createPipe "python ./app/wrapper.py"),
          freeResource = \p -> do
            hClose (getStdin p)
            stopProcess p,
          poolCacheTTL = 3600,
          poolMaxResources = count
        }
