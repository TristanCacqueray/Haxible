-- | This module contains the evaluation logic
module Haxible.Eval
  ( AnsibleHaxl,
    runHaxible,
    runTask,
    json,
    envLoop,
    traverseLoop,
    traverseInclude,
    cleanVar,
    Value,
    Vars,
  )
where

import Data.Aeson hiding (json)
import Data.Aeson.KeyMap qualified
import Data.Aeson.QQ
import Data.Default (def)
import Data.Functor.Identity (runIdentity)
import Data.Vector qualified
import Haxible.Connection
import Haxible.DataSource
import Haxible.Prelude
import Haxl.Core hiding (env)
import Language.Haskell.TH.Quote qualified
import Text.Ginger

type Vars = [(Text, Value)]

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
    template = case runIdentity (parseGinger (const undefined) Nothing (from n)) of
      Right tmpl -> tmpl
      Left e -> error $ "Template fail: " <> show e

traverseLoop :: Applicative f => (a -> f Value) -> [a] -> f Value
traverseLoop f xs = loopResult <$> traverse f xs

traverseInclude :: Applicative f => (a -> f [Value]) -> [a] -> f [Value]
traverseInclude f xs = concat <$> traverse f xs

loopResult :: [Value] -> Value
loopResult xs = Object $ Data.Aeson.KeyMap.fromList attrs
  where
    attrs =
      [ ("results", Array (Data.Vector.fromList $ cleanVar <$> xs)),
        ("msg", "All items completed"),
        ("skipped", Bool $ all (fromMaybe False . preview (key "skipped" . _Bool)) xs),
        ("changed", Bool $ any (fromMaybe False . preview (key "changed" . _Bool)) xs)
      ]

runTask :: [(Text, Value)] -> Text -> Value -> [(Text, Value)] -> AnsibleHaxl Value
runTask playAttrs module_ taskObject baseEnv = dataFetch (RunTask (TaskCall {playAttrs, taskObject, env, module_}))
  where
    env = concatMap checkManyHost baseEnv
    -- When a task run on many host, we register a single variable with all the results,
    -- thus when accessing the variable, we need to lookup the current host result.
    checkManyHost (k, v)
      | preview (key "__haxible_many_hosts" . _Bool) v == Just True =
          let many_k = k <> "__haxible"
           in [ (k, String $ "{{ " <> many_k <> "[ansible_host] }}"),
                (many_k, cleanVar v)
              ]
      | otherwise = [(k, cleanVar v)]

runHaxible :: FilePath -> AnsibleHaxl [Value] -> IO ()
runHaxible inventory action = withConnections 5 inventory $ \connections -> do
  ansibleState <- initHaxibleState connections
  ansibleEnv <- initEnv (stateSet ansibleState stateEmpty) ()
  xs <- runHaxl ansibleEnv action
  putStrLn "\nReport:"
  traverse_ printResult xs
  putStrLn "Done."
  where
    printResult :: Value -> IO ()
    printResult v = putStrLn $ "[ok]: " <> unsafeFrom (encode v)
