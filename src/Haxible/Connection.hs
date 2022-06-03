{-# LANGUAGE DeriveAnyClass #-}

-- | This module contains the logic to interact with the python interpreter
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
  { playAttrs :: [(Text, Value)],
    taskObject :: Value,
    env :: [(Text, Value)]
  }
  deriving (Eq, Show, Typeable, Generic, Hashable)

-- | Python calls takes a list of action and attribute, and it produces a list of result.
newtype Connections = Connections {run :: [TaskCall] -> IO [(Int, Value)]}

mkObj :: [(Text, Value)] -> Value
mkObj = Object . Data.Aeson.KeyMap.fromList . map (first Data.Aeson.Key.fromText)

-- | Creates the Python interpreters.
withConnections :: Int -> (Connections -> IO ()) -> IO ()
withConnections count callback =
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

      let cb :: [TaskCall] -> IO [(Int, Value)]
          cb tasks = do
            traverse (Data.Pool.withResource pool . runTask) tasks

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
