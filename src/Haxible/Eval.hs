-- | This module contains the evaluation logic
module Haxible.Eval
  ( AnsibleHaxl,
    runHaxible,
    runTask,
    json,
  )
where

import Data.Aeson hiding (json)
import Data.Aeson.QQ
import Data.Text
import Haxible.Connection
import Haxible.DataSource
import Haxl.Core hiding (env)
import Language.Haskell.TH.Quote qualified

json :: Language.Haskell.TH.Quote.QuasiQuoter
json = aesonQQ

runTask :: Text -> Maybe Text -> Text -> Value -> [(Text, Value)] -> AnsibleHaxl Value
runTask host name task attrs env = dataFetch (RunTask (TaskCall {host, name, task, attrs, env}))

runHaxible :: AnsibleHaxl () -> IO ()
runHaxible action = withConnections 5 $ \connections -> do
  ansibleState <- initHaxibleState connections
  ansibleEnv <- initEnv (stateSet ansibleState stateEmpty) ()
  runHaxl ansibleEnv action
