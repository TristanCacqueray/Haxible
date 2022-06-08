-- | This module contains the evaluation logic
module Haxible.Eval
  ( AnsibleHaxl,
    runHaxible,
    runTask,
    json,
    envLoop,
    extractFact,
    traverseLoop,
    traverseInclude,
    cleanVar,
    tryRescue,
    Value,
    Vars,
  )
where

import Data.Aeson hiding (json)
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified
import Data.Default (def)
import Data.Functor.Identity (runIdentity)
import Data.Vector qualified
import Haxible.Connection
import Haxible.DataSource
import Haxible.Prelude
import Haxible.Report
import Haxl.Core hiding (env)
import Text.Ginger

type BlockFun = Vars -> Vars -> AnsibleHaxl [Value]

-- $setup
-- >>> let dump = putStrLn . unsafeFrom . encode

-- | Call the rescue block if the main block fails.
tryRescue :: BlockFun -> BlockFun -> BlockFun
tryRescue main rescue a b = do
  res <- tryToHaxlException (main a b)
  case res of
    Left _ -> rescue a b
    Right x -> pure x

-- | Evaluate template for the loop attribute when it is a string (instead of a list):
--
-- >>> envLoop "{{ hosts }}" [("hosts", [json|["frontend", "backend"]|])]
-- [String "frontend",String "backend"]
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

-- | Build the loop results:
--
-- >>> dump =<< traverseLoop (pure . String) ["a", "b"]
-- {"changed":false,"msg":"All items completed","results":["a","b"],"skipped":false}
traverseLoop :: Applicative f => (a -> f Value) -> [a] -> f Value
traverseLoop f xs = loopResult <$> traverse f xs

-- | Concat nested results
--
-- >>> dump =<< traverseInclude (\x -> pure [String x, Number 42]) ["a", "b"]
-- ["a",42,"b",42]
traverseInclude :: Applicative f => (a -> f [Value]) -> [a] -> f [Value]
traverseInclude f xs = concat <$> traverse f xs

-- | Extract the ansible_facts results
--
-- >>> dump $ extractFact [json|{"ansible_facts": {"key": "value"}}|]
-- "value"
--
-- Each fact are evaluated individually, so multiple result is un-expected:
--
-- >>> extractFacts [json|{"ansible_facts": {"x": 1, "y": 2}}|]
-- Left "Multiple facts found: [(\"x\",Number 1.0),(\"y\",Number 2.0)]"
extractFacts :: Value -> Either String Value
extractFacts v = case preview (key "ansible_facts") v of
  Just (Object obj) -> case Data.Aeson.KeyMap.toList obj of
    [(_, value)] -> Right value
    xs -> Left $ "Multiple facts found: " <> show xs
  _ -> case preview (key "skip_reason" . _String) v of
    Just _ -> Right Null
    Nothing -> Left $ "Can't find ansible_facts in " <> unsafeFrom (encode v)

extractFact :: Value -> Value
extractFact v = case extractFacts v of
  Left e -> error e
  Right x -> x

-- | Process loop results
--
-- >>> dump $ loopResult [[json|42|], [json|43|]]
-- {"changed":false,"msg":"All items completed","results":[42,43],"skipped":false}
loopResult :: [Value] -> Value
loopResult xs = Object $ Data.Aeson.KeyMap.fromList attrs
  where
    play = case xs of
      (x : _) -> copyKey "__haxible_play" x <> copyKey "__haxible_module" x
      _ -> []
    copyKey k x = maybe [] (\p -> [(Data.Aeson.Key.fromText k, p)]) (preview (key k) x)
    attrs =
      [ ("results", Array (Data.Vector.fromList xs)),
        ("msg", "All items completed"),
        ("skipped", Bool $ all (fromMaybe False . preview (key "skipped" . _Bool)) xs),
        ("changed", Bool $ any (fromMaybe False . preview (key "changed" . _Bool)) xs)
      ]
        <> play

runTask :: Vars -> Text -> Value -> Vars -> Vars -> AnsibleHaxl Value
runTask playAttrs module_ moduleObject taskAttrs baseTaskVars =
  addModule <$> dataFetch (RunTask (TaskCall {playAttrs, moduleObject, taskAttrs, taskVars, module_}))
  where
    addModule = \case
      Object obj -> Object $ Data.Aeson.KeyMap.insert "__haxible_module" (String module_) obj
      x -> x
    taskVars = filter ((/=) Null . snd) (concatMap checkManyHost baseTaskVars)
    -- When a task run on many host, we register a single variable with all the results,
    -- thus when accessing the variable, we need to lookup the current host result.
    checkManyHost (k, v)
      | preview (key "__haxible_multi_hosts" . _Bool) v == Just True =
          let many_k = k <> "__haxible"
           in [ (k, String $ "{{ " <> many_k <> "[ansible_host] }}"),
                (many_k, cleanVar v)
              ]
      | otherwise = [(k, cleanVar v)]

runHaxible :: FilePath -> FilePath -> AnsibleHaxl [Value] -> IO ()
runHaxible inventory playPath action = withConnections 5 inventory $ \connections -> do
  ansibleState <- initHaxibleState connections
  ansibleEnv <- initEnv (stateSet ansibleState stateEmpty) ()
  xs <- runHaxl ansibleEnv action
  traverse_ (putStrLn . unsafeFrom . encode) xs
  case nonEmpty xs of
    Just res -> do
      writeFile (playName <> ".plantuml") (from $ Haxible.Report.reportTiming res)
    Nothing -> putStrLn "\nEmpty results :("
  where
    (playName, _) = splitExtension playPath
