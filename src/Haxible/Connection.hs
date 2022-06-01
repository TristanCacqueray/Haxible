-- | This module contains the logic to interact with the python interpreter
module Haxible.Connection (Python (..), withConnection) where

import Data.Aeson (Value, eitherDecodeStrict, encode)
import Data.ByteString (hGetLine, toStrict)
import Data.ByteString.Char8 (hPutStrLn)
import Data.Text (Text)
import GHC.IO.Handle (hFlush)
import System.IO (hClose)
import System.Process.Typed

-- | Python calls takes a JSON value and produces a JSON value.
newtype Python = Python {call :: [(Text, Value)] -> IO [Value]}

-- | Creates the Python interpreter.
withConnection :: (Python -> IO ()) -> IO ()
withConnection go = do
  let wrapperConfig = setStdin createPipe $ setStdout createPipe "python ./app/wrapper.py"
  withProcessTerm wrapperConfig $ \p -> do
    putStrLn "Process started!"
    let cb :: [(Text, Value)] -> IO [Value]
        cb tasks = do
          hPutStrLn (getStdin p) (toStrict $ encode tasks)
          hFlush (getStdin p)
          output <- hGetLine (getStdout p)
          case eitherDecodeStrict output of
            Right res -> pure res
            Left err -> error $ show output <> ": " <> err
    go (Python cb)
    hClose (getStdin p)
