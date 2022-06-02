-- | This module contains the Haxl logic
module Haxible.DataSource (AnsibleHaxl, initHaxibleState, dataFetch, TaskReq (..)) where

import Control.Concurrent.Async (async)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (Value)
import Data.Hashable (Hashable (hashWithSalt))
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Haxible.Connection (Connections (..), TaskCall (..))
import Haxl.Core
import Say

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
    say $ "[+] Batching " <> Text.pack (show (List.length reqs)) <> " tasks"
    fetchAsync state.connections reqs

fetchAsync :: Connections -> [BlockedFetch TaskReq] -> IO ()
fetchAsync python xs =
  void $
    async $ do
      let tasksResults :: [ResultVar Value]
          tasks :: [TaskCall]
          (tasks, tasksResults) =
            unzip
              [(task, r) | BlockedFetch (RunTask task) r <- xs]

      -- TODO: spawn a wrapper per host
      resultsE <- Control.Exception.try $ python.run tasks
      case resultsE of
        Left ex -> mapM_ (\(BlockedFetch _ rvar) -> putFailure rvar (ex :: SomeException)) xs
        Right results -> mapM_ (\(res, rvar) -> putSuccess rvar res) (zip results tasksResults)
