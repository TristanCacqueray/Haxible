-- | This module contains the Haxl logic
module Haxible.DataSource (AnsibleHaxl, initHaxibleState, dataFetch, TaskReq (..)) where

import Control.Concurrent.Async (async)
import Control.Exception (Exception, SomeException, try)
import Control.Lens
import Data.Aeson.Lens
import Data.Hashable (Hashable (hashWithSalt))
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Haxible.Connection (Connections (..), TaskCall (..))
import Haxible.Prelude hiding (State, state)
import Haxl.Core
import Say
import System.Clock qualified as Clock

type AnsibleHaxl a = GenHaxl () () a

data TaskReq a where
  RunTask :: TaskCall -> TaskReq Value
  deriving (Typeable)

deriving instance Eq (TaskReq a)

deriving instance Show (TaskReq a)

instance ShowP TaskReq where showp = show

instance Hashable (TaskReq a) where
  hashWithSalt s (RunTask param) = hashWithSalt s (0 :: Int, param)

instance StateKey TaskReq where
  data State TaskReq = AnsibleState {connections :: Connections}

instance DataSourceName TaskReq where
  dataSourceName _ = "Ansible"

instance DataSource u TaskReq where
  fetch = fetchTask

initHaxibleState :: Connections -> IO (State TaskReq)
initHaxibleState connections = pure $ AnsibleState {connections}

fetchTask :: State TaskReq -> Flags -> u -> PerformFetch TaskReq
fetchTask state _flags _user =
  BackgroundFetch $ \reqs -> do
    debug <- lookupEnv "HAXIBLE_DEBUG"
    case debug of
      Just _ -> say $ "▶ Batching " <> Text.pack (show (List.length reqs)) <> " tasks"
      Nothing -> pure ()
    now <- Clock.toNanoSecs <$> Clock.getTime Clock.Monotonic
    traverse_ (fetchAsync state.connections now) reqs

data TaskError = TaskError Int Value
  deriving (Show)

instance Exception TaskError

fetchAsync :: Connections -> Integer -> BlockedFetch TaskReq -> IO ()
fetchAsync python ts (BlockedFetch (RunTask task) rvar) =
  void $
    async $ do
      resultsE <- Control.Exception.try $ python.run task
      now <- Clock.toNanoSecs <$> Clock.getTime Clock.Monotonic
      case resultsE of
        Left ex -> putFailure rvar (ex :: SomeException)
        Right (0, result) -> putSuccess rvar (addTS now result)
        Right (code, res) -> putFailure rvar (TaskError code (addTS now res))
  where
    addTS now =
      (_Object . at "__haxible_start" ?~ Number (fromInteger ts))
        . (_Object . at "__haxible_end" ?~ Number (fromInteger now))
