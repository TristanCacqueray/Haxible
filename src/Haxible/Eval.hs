-- | This module contains the evaluation logic
module Haxible.Eval
  ( AnsibleHaxl,
    runHaxible,
    runTask,
    renderTemplates,
    json,
  )
where

import Data.Aeson hiding (json)
import Data.Aeson.QQ
import Data.Default (def)
import Data.Functor.Identity (runIdentity)
import Data.Text
import Haxible.Connection
import Haxible.DataSource
import Haxl.Core
import Language.Haskell.TH.Quote qualified
import Text.Ginger

json :: Language.Haskell.TH.Quote.QuasiQuoter
json = aesonQQ

runTask :: Text -> Maybe Text -> Text -> Value -> AnsibleHaxl Value
runTask _host name action attr = dataFetch (RunTask name action attr)

applyTemplate :: [(Text, Value)] -> Text -> Text
applyTemplate vars src = runGinger context template
  where
    context = makeContextText $ \var -> maybe def toGVal (lookup var vars)
    template :: Template SourcePos
    template = case runIdentity (parseGinger (const undefined) Nothing (unpack src)) of
      Right tmpl -> tmpl
      Left e -> error $ "Template fail: " <> show e

renderTemplates :: [(Text, Value)] -> Value -> Value
renderTemplates vars = go
  where
    go :: Value -> Value
    go v = case v of
      String x -> String (applyTemplate vars x)
      Object x -> Object (go <$> x)
      Array xs -> Array (go <$> xs)
      _ -> v

runHaxible :: AnsibleHaxl () -> IO ()
runHaxible action = withConnection $ \python -> do
  ansibleState <- initHaxibleState 1 python
  ansibleEnv <- initEnv (stateSet ansibleState stateEmpty) ()
  runHaxl ansibleEnv action
