-- | This module contains the Haxl logic
module Haxible.DataSource (AnsibleHaxl, initHaxibleState, dataFetch, TaskReq (..), TaskParam (..)) where

import Control.Concurrent.Async (async)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (Value)
import Data.Hashable (Hashable (hashWithSalt))
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Haxible.Connection (Python (..))
import Haxl.Core
import Say

type AnsibleHaxl a = GenHaxl () () a

data TaskParam = TaskParam
  { host :: Text,
    name :: Maybe Text,
    -- task is a module or action name
    task :: Text,
    attrs :: Value
  }
  deriving (Eq, Show, Typeable)

data TaskReq a where
  RunTask :: TaskParam -> TaskReq Value
  deriving (Typeable)

deriving instance Eq (TaskReq a)

deriving instance Show (TaskReq a)

instance ShowP TaskReq where showp = show

instance Hashable (TaskReq a) where
  hashWithSalt s (RunTask param) = hashWithSalt s (0 :: Int, param.host, param.task, param.attrs)

instance StateKey TaskReq where
  data State TaskReq = AnsibleState {python :: Python}

instance DataSourceName TaskReq where
  dataSourceName _ = "Ansible"

instance DataSource u TaskReq where
  fetch = fetchTask

initHaxibleState :: Python -> IO (State TaskReq)
initHaxibleState python = do
  pure $ AnsibleState {python}

groupTasksByHost :: [BlockedFetch TaskReq] -> [[BlockedFetch TaskReq]]
groupTasksByHost = List.groupBy groupByHost . List.sortBy compareHost
  where
    compareHost t1 t2 = compare (getReq t1).host (getReq t2).host
    groupByHost t1 t2 = (getReq t1).host == (getReq t2).host

getReq :: BlockedFetch TaskReq -> TaskParam
getReq (BlockedFetch (RunTask req) _) = req

fetchTask :: State TaskReq -> Flags -> u -> PerformFetch TaskReq
fetchTask state _flags _user =
  BackgroundFetch $ \reqs -> do
    say $ "[+] Batching " <> Text.pack (show (List.length reqs)) <> " tasks"
    mapM_ (fetchAsync state.python) (groupTasksByHost reqs)

fetchAsync :: Python -> [BlockedFetch TaskReq] -> IO ()
fetchAsync python xs =
  void $
    async $ do
      let tasksResults :: [ResultVar Value]
          tasks :: [TaskParam]
          (tasks, tasksResults) =
            unzip
              [(task, r) | BlockedFetch (RunTask task) r <- xs]

      -- TODO: spawn a wrapper per host
      resultsE <- Control.Exception.try $ runTaskReq python tasks
      case resultsE of
        Left ex -> mapM_ (\(BlockedFetch _ rvar) -> putFailure rvar (ex :: SomeException)) xs
        Right results -> mapM_ (\(res, rvar) -> putSuccess rvar res) (zip results tasksResults)

runTaskReq :: Python -> [TaskParam] -> IO [Value]
runTaskReq python xs = do
  say $ " ▶ Calling " <> Text.pack (show actions)
  python.call actions
  where
    actions = [(task, attrs) | TaskParam _host _name task attrs <- xs]
