-- | This module contains the logic to interact with the python wrapper
module Haxible.Connection (Connections (..), TaskCall (..), withConnections, cleanVar) where

import Control.Exception (bracket)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.Aeson.KeyMap qualified
import Data.ByteString (hGetLine, toStrict)
import Data.ByteString.Char8 (hPutStrLn)
import Data.Hashable (Hashable)
import Data.Pool qualified
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import GHC.IO.Handle (hFlush)
import Haxible.Prelude
import Say
import System.Console.ANSI (Color (..), ColorIntensity (Dull), ConsoleLayer (Foreground), hSupportsANSIColor, setSGRCode)
import System.Console.ANSI.Codes (SGR (..))
import System.IO (Handle, hClose, stdout)
import System.Process (Pid, getPid)
import System.Process.Typed

data TaskCall = TaskCall
  { -- | The playbook attributes, such as `hosts` or `become`.
    playAttrs :: [(Text, Value)],
    -- | The module name for debug purpose, it is more convenient to access than reading it from the task object.
    module_ :: Text,
    -- | The task object, e.g `{"file": {"path": "/etc/zuul"}}`.
    taskObject :: Value,
    -- | A list of variables to be added to play vars, e.g. for the attribute template values.
    playVars :: [(Text, Value)]
  }
  deriving (Eq, Show, Typeable, Generic, Hashable)

-- | A connection run converts a TaskCall into a (result code, result value)
newtype Connections = Connections {run :: TaskCall -> IO (Int, Value)}

-- | Add horizontal line separator
-- >>> addSep 10 "TASK"
-- "TASK ****"
addSep :: Int -> Text -> Text
addSep width x = x <> " " <> sep
  where
    sep = Text.replicate (width - Text.length x - 2) "*"

-- | Format process id.
-- >>> formatPid 42
-- "<42>"
formatPid :: Pid -> Text
formatPid pid = "<" <> from (show pid) <> ">"

formatTask :: Pid -> TaskCall -> Text
formatTask pid tc = "TASK [" <> name <> "] " <> formatPid pid
  where
    name = fromMaybe tc.module_ (preview (key "name" . _String) tc.taskObject)

formatResult :: Bool -> Pid -> (Int, Value) -> String
formatResult withColor pid (code, val) = pre <> from txt <> post
  where
    (pre, post)
      | withColor = (setSGRCode [SetColor Foreground Dull resColor], setSGRCode [Reset])
      | otherwise = ("", "")
    txt = res <> ": [" <> host <> "] " <> formatPid pid <> " => " <> jsonDump <> "\n"
    jsonDump = decodeUtf8 . from . encode . cleanVar $ val
    host = fromMaybe "unknown?!" (preview (key "__haxible_play" . key "hosts" . _String) val)
    resColor
      | code /= 0 = Red
      | otherwise = Green
    res
      | code == 0 = "ok"
      | otherwise = from (show code)

cleanVar :: Value -> Value
cleanVar = \case
  Object obj -> Object $ Data.Aeson.KeyMap.filterWithKey (\k _ -> k `notElem` addedKey) obj
  x -> x
  where
    -- TODO: keep in sync with the wrapper and the data source
    addedKey =
      ["__haxible_play", "__haxible_start", "__haxible_end", "__haxible_module"]
        <> ["_ansible_no_log", "_ansible_verbose_always"]

-- | Creates the Python interpreters.
withConnections :: Int -> FilePath -> (Connections -> IO ()) -> IO ()
withConnections count inventory callback =
  bracket (Data.Pool.newPool poolConfig) Data.Pool.destroyAllResources go
  where
    go pool = do
      termWidth <- maybe 80 read <$> lookupEnv "COLUMNS"
      withColor <- hSupportsANSIColor stdout
      say (addSep termWidth "PLAY [concurrent]" <> "\n")

      let runTask :: TaskCall -> Process Handle Handle () -> IO (Int, Value)
          runTask taskCall p = do
            let callParams = [mkObj taskCall.playAttrs, taskCall.taskObject, mkObj taskCall.playVars]
            pid <- fromMaybe (error "no pid?!") <$> getPid (unsafeProcessHandle p)
            say (addSep termWidth (formatTask pid taskCall))
            hPutStrLn (getStdin p) (toStrict $ encode callParams)
            hFlush (getStdin p)
            output <- hGetLine (getStdout p)
            case eitherDecodeStrict output of
              Right res -> do
                sayString (formatResult withColor pid res)
                pure res
              Left err -> error $ show output <> ": " <> err

      callback (Connections $ Data.Pool.withResource pool . runTask)

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
