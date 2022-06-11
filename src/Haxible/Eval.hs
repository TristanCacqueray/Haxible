-- | This module contains the evaluation logic
module Haxible.Eval
  ( AnsibleHaxl,
    runHaxible,
    runTask,
    notifyHandler,
    json,
    extractLoop,
    extractFact,
    extractWhen,
    traverseLoop,
    traverseInclude,
    cleanVar,
    tryRescue,
    Value,
    Vars,
  )
where

import Control.Exception (handle)
import Data.Aeson hiding (json)
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified
import Data.Vector qualified
import Haxible.Connection
import Haxible.DataSource
import Haxible.Prelude
import Haxible.Report
import Haxl.Core hiding (env)

type BlockFun = Vars -> AnsibleHaxl [Value]

-- $setup
-- >>> let dump = putStrLn . unsafeFrom . encode

-- | Call the rescue block if the main block fails.
tryRescue :: BlockFun -> BlockFun -> BlockFun
tryRescue main rescue localVars = do
  res <- tryToHaxlException (main localVars)
  case res of
    Left _ -> rescue localVars
    Right x -> pure x

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

extractWhen :: Value -> Bool
extractWhen v = case preview (key "msg") v of
  Just (Bool x) -> x
  _ -> error $ "Can't find bool msg: " <> unsafeFrom (encode v)

extractLoop :: Value -> [Value]
extractLoop v = case preview (key "msg") v of
  Just (Array xs) -> toList xs
  _ -> error $ "Can't find list msg: " <> unsafeFrom (encode v)

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
      (x : _) -> copyKey "__haxible_play" x <> copyKey "__haxible_module" x <> copyKey "__haxible_notify" x
      _ -> []
    copyKey k x = maybe [] (\p -> [(Data.Aeson.Key.fromText k, p)]) (preview (key k) x)
    attrs =
      [ ("results", Array (Data.Vector.fromList xs)),
        ("msg", "All items completed"),
        ("skipped", Bool $ all (fromMaybe False . preview (key "skipped" . _Bool)) xs),
        ("changed", Bool $ any (fromMaybe False . preview (key "changed" . _Bool)) xs)
      ]
        <> play

notifyHandler :: Vars -> [Value] -> Text -> Text -> Vars -> AnsibleHaxl ()
notifyHandler playAttrs res handler module_ taskAttrs
  | handlerMatch =
      void $ dataFetch (RunTask TaskCall {taskPath, playAttrs, taskAttrs, module_})
  | otherwise = pure ()
  where
    taskPath = ""
    handlerMatch = any notifyMatchListen res
    notifyMatchListen v = case (preview (key "changed" . _Bool) v, preview (key "__haxible_notify") v) of
      (Just True, Just (String s)) -> s == handler
      _ -> False

runTask :: FilePath -> Vars -> Vars -> Text -> Vars -> Vars -> AnsibleHaxl Value
runTask taskPath basePlayAttrs defaultVars module_ baseTaskAttrs baseLocalVars =
  addNotify . addModule <$> dataFetch (RunTask (TaskCall {taskPath, playAttrs, taskAttrs, module_}))
  where
    playAttrs = ("vars", playVarsObj) : basePlayAttrs
    playVarsObj = case lookup "vars" basePlayAttrs of
      Nothing -> mkObj (localVars <> defaultVars)
      Just (Object obj) -> Object (mkVars obj)
      Just x -> error $ "Invalid play vars: " <> unsafeFrom (encode x)
    mkVars playVars =
      -- Combine localVars > playVars > defaultVars
      Data.Aeson.KeyMap.union (Data.Aeson.KeyMap.union (mkKeyMap localVars) playVars) (mkKeyMap defaultVars)

    taskAttrs = filter ((/=) "notify" . fst) baseTaskAttrs
    addNotify = case lookup "notify" baseTaskAttrs of
      Just n -> \case
        Object obj -> Object $ Data.Aeson.KeyMap.insert "__haxible_notify" n obj
        x -> error $ "Can't add notify to: " <> unsafeFrom (encode x)
      Nothing -> id

    addModule = \case
      Object obj -> Object $ Data.Aeson.KeyMap.insert "__haxible_module" (String module_) obj
      x -> x

    localVars = filter ((/=) Null . snd) (concatMap checkManyHost baseLocalVars)
    -- When a task run on many host, we register a single variable with all the results,
    -- thus when accessing the variable, we need to lookup the current host result.
    checkManyHost (k, v)
      | preview (key "__haxible_multi_hosts" . _Bool) v == Just True =
          let many_k = k <> "__haxible"
           in [ (k, String $ "{{ " <> many_k <> "[ansible_host] }}"),
                (many_k, cleanVar v)
              ]
      | otherwise = [(k, cleanVar v)]

runHaxible :: FilePath -> FilePath -> [Value] -> AnsibleHaxl [Value] -> IO ()
runHaxible inventory playPath expected action = withConnections 5 inventory playPath $ \connections -> do
  ansibleState <- initHaxibleState connections
  ansibleEnv <- initEnv (stateSet ansibleState stateEmpty) ()
  xs <- handle printError (runHaxl ansibleEnv action)
  case expected of
    [] -> printReport xs
    _ -> do
      traverse_ assertResults (zip expected (cleanVar <$> xs))
      when (length expected /= length xs) $ error $ "Expected " <> show (length expected) <> ", got: " <> show (length xs)
  where
    printReport xs = do
      traverse_ (putStrLn . unsafeFrom . encode) xs
      case nonEmpty xs of
        Just res -> do
          writeFile (playName <> ".plantuml") (from $ Haxible.Report.reportTiming res)
        Nothing -> putStrLn "\nEmpty results :("
    (playName, _) = splitExtension playPath
    printError (TaskError code v) = do
      error $ "Error: " <> show code <> ": " <> unsafeFrom (encode v)

    assertResults (expect, result)
      | expect /= result = error $ "Expected\n  " <> unsafeFrom (encode expect) <> ", but got\n  " <> unsafeFrom (encode result)
      | otherwise = pure ()
