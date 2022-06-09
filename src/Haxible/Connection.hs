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
  { -- | The directory where the task is defined, to enable python library path
    taskPath :: FilePath,
    -- | The playbook attributes, such as `hosts` or `become`.
    playAttrs :: Vars,
    -- | The module name for debug purpose, it is more convenient to access than reading it from the moduleObject.
    module_ :: Text,
    -- | The task attributes
    taskAttrs :: Vars,
    -- | Extra task vars, e.g. role defaults
    taskVars :: Vars
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
formatTask pid tc = "TASK " <> formatPid pid <> " [" <> name <> "]"
  where
    name = fromMaybe tc.module_ (preview _String =<< lookup "name" tc.taskAttrs)

formatResult :: Bool -> Pid -> (Int, Value) -> String
formatResult withColor pid (code, val) = pre <> from txt <> post
  where
    (pre, post)
      | withColor = (setSGRCode [SetColor Foreground Dull resColor], setSGRCode [Reset])
      | otherwise = ("", "")
    txt = res <> ": [" <> host <> "] " <> formatPid pid <> " => " <> jsonDump <> "\n"
    jsonDump = decodeUtf8 . from . encode . cleanVar $ val
    host = fromMaybe "unknown?!" (preview (key "__haxible_play" . key "hosts" . _String) val)
    skipped = isJust (preview (key "skip_reason") val)
    changed = fromMaybe False (preview (key "changed" . _Bool) val)
    (res, resColor)
      | code /= 0 = ("failed", Red)
      | skipped = ("skipping", Cyan)
      | changed = ("changed", Yellow)
      | otherwise = ("ok", Green)

cleanVar :: Value -> Value
cleanVar = \case
  Object obj -> Object $ cleanVar <$> Data.Aeson.KeyMap.filterWithKey (\k _ -> k `notElem` addedKey) obj
  Array xs -> Array $ cleanVar <$> xs
  x -> x
  where
    -- TODO: keep in sync with the wrapper and the data source
    addedKey =
      ["__haxible_play", "__haxible_start", "__haxible_end", "__haxible_module", "__haxible_notify"]
        <> ["__haxible_multi_hosts"]
        <> ["_ansible_no_log", "_ansible_verbose_always", "__ansible_delegated_vars"]

-- | Creates the Python interpreters.
withConnections :: Int -> FilePath -> FilePath -> (Connections -> IO ()) -> IO ()
withConnections count inventory playPath callback =
  bracket (Data.Pool.newPool poolConfig) Data.Pool.destroyAllResources go
  where
    go pool = do
      termWidth <- maybe 80 read <$> lookupEnv "COLUMNS"
      withColor <- hSupportsANSIColor stdout
      say (addSep termWidth "PLAY [concurrent]" <> "\n")

      let runTask :: TaskCall -> Process Handle Handle () -> IO (Int, Value)
          runTask taskCall p = do
            let callParams =
                  [ String (from taskCall.taskPath),
                    mkObj taskCall.playAttrs,
                    mkObj taskCall.taskAttrs,
                    mkObj taskCall.taskVars
                  ]
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
              $ proc "python" ["./app/wrapper.py", inventory, playPath],
          freeResource = \p -> do
            hClose (getStdin p)
            stopProcess p,
          poolCacheTTL = 3600,
          poolMaxResources = count
        }
