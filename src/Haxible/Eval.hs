-- | This module contains the evaluation logic
module Haxible.Eval
  ( AnsibleHaxl,
    runHaxible,
    runTask,
    json,
    envLoop,
  )
where

import Data.Aeson hiding (json)
import Data.Aeson.QQ
import Data.Default (def)
import Data.Functor.Identity (runIdentity)
import Data.Text
import Haxible.Connection
import Haxible.DataSource
import Haxl.Core hiding (env)
import Language.Haskell.TH.Quote qualified
import Text.Ginger

json :: Language.Haskell.TH.Quote.QuasiQuoter
json = aesonQQ

envLoop :: Text -> [(Text, Value)] -> [Value]
envLoop n env = runGinger context template
  where
    lookupVar var = maybe def toGVal (lookup var env)
    gvarToList g = case g.asList of
      Just xs -> case traverse fromGValEither xs of
        Right x -> x
        Left err -> error $ "Couldn't resolve loop list: " <> err
      Nothing -> error $ "Invalid loop variable: " <> show g

    context = makeContext' lookupVar gvarToList Nothing
    template :: Template SourcePos
    template = case runIdentity (parseGinger (const undefined) Nothing (unpack n)) of
      Right tmpl -> tmpl
      Left e -> error $ "Template fail: " <> show e

runTask :: Text -> Maybe Text -> Text -> Value -> [(Text, Value)] -> AnsibleHaxl Value
runTask host name task attrs env = dataFetch (RunTask (TaskCall {host, name, task, attrs, env}))

runHaxible :: AnsibleHaxl () -> IO ()
runHaxible action = withConnections 5 $ \connections -> do
  ansibleState <- initHaxibleState connections
  ansibleEnv <- initEnv (stateSet ansibleState stateEmpty) ()
  runHaxl ansibleEnv action
