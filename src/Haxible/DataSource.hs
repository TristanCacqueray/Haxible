-- | This module contains the Haxl logic
module Haxible.DataSource (AnsibleHaxl, initHaxibleState, dataFetch, TaskReq (..)) where

import Control.Concurrent (QSem, newQSem, signalQSem, waitQSem)
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, bracket_, try)
import Control.Monad (void)
import Data.Aeson (Value)
import Data.Hashable (Hashable (hashWithSalt))
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Haxible.Connection (Python (..))
import Haxl.Core
import Say

type AnsibleHaxl a = GenHaxl () () a

data TaskReq a where
  RunTask :: Maybe Text -> Text -> Value -> TaskReq Value
  deriving (Typeable)

deriving instance Eq (TaskReq a)

deriving instance Show (TaskReq a)

instance ShowP TaskReq where showp = show

instance Hashable (TaskReq a) where
  hashWithSalt s (RunTask _ n v) = hashWithSalt s (0 :: Int, n, v)

instance StateKey TaskReq where
  data State TaskReq = AnsibleState
    { python :: Python,
      semaphore :: QSem
    }

instance DataSourceName TaskReq where
  dataSourceName _ = "Ansible"

instance DataSource u TaskReq where
  fetch = fetchTask

initHaxibleState :: Int -> Python -> IO (State TaskReq)
initHaxibleState threads python = do
  semaphore <- newQSem threads
  pure $ AnsibleState {python, semaphore}

fetchTask :: State TaskReq -> Flags -> u -> PerformFetch TaskReq
fetchTask state _flags _user =
  BackgroundFetch $ \reqs -> do
    say $ "[+] Batching " <> Text.pack (show (List.length reqs)) <> " tasks"
    mapM_ (fetchAsync state.python state.semaphore) reqs

fetchAsync :: Python -> QSem -> BlockedFetch TaskReq -> IO ()
fetchAsync python sem (BlockedFetch req rvar) =
  void $
    async $
      bracket_ (waitQSem sem) (signalQSem sem) $ do
        e <- Control.Exception.try $ runTaskReq python req
        case e of
          Left ex -> putFailure rvar (ex :: SomeException)
          Right a -> putSuccess rvar a

runTaskReq :: Python -> TaskReq a -> IO a
runTaskReq python (RunTask name action attr) = do
  say $ " ▶ Calling " <> fromMaybe "unnamed task" name
  python.call action attr
