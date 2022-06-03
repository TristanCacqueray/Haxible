module Haxible.Parser (decodePlaybook, renderScript) where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified
import Data.Aeson.Lens
import Data.Bifunctor (first)
import Data.ByteString (toStrict)
import Data.Foldable
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text
import Data.Text.Encoding
import Haxible.Play
import System.FilePath

objToVar :: Value -> [(Text, Value)]
objToVar v = case v of
  Object obj -> first Data.Aeson.Key.toText <$> Data.Aeson.KeyMap.toList obj
  _ -> error $ "Expecting object, got " <> show v

decodePlaybook :: FilePath -> IO Playbook
decodePlaybook fp =
  Playbook <$> (traverse (resolveHostPlay fp) =<< decodeFile fp)

-- | This function is the core of haxible. Given a list of task, it populates the task.requires attributes.
annotateDependency :: [Task] -> [Task]
annotateDependency = reverse . fst . foldl' go ([], [])
  where
    go :: ([Task], [Dependency]) -> Task -> ([Task], [Dependency])
    go (xs, available) task = (task {requires, register, vars} : xs, newAvailable)
      where
        -- look in all string value to see if an available is used.
        findRequirements :: Value -> [Dependency]
        findRequirements v = case v of
          String x -> case filter (\n -> dependencyName n `Text.isInfixOf` x) available of
            [] -> []
            requirement -> requirement
          Object x -> concatMap findRequirements x
          Array x -> concatMap findRequirements x
          _ -> []

        requires = concatMap findRequirements (task.loop : task.tmodule.params : map snd task.vars)
        taskRegister =
          -- If there is no register but a file destination, then add a fake register
          task.register <|> dependencyVar <$> dest

        newAvailable = reg <> maybeToList dest <> varModule <> available
        reg = maybeToList $ Register <$> task.register

        getAttr n = preview (key n . _String) (task.tmodule.params)
        dest = case getAttr "path" <|> getAttr "dest" of
          Just n -> Just $ case task.register of
            -- re-use the register named
            Just r -> Path r (Text.unpack n)
            -- add a fake register value
            Nothing -> Path (Text.replace "/" "_" $ Text.replace "." "_" n) (Text.unpack n)
          Nothing -> Nothing

        -- include_vars and set_facts provide ad-hoc variables
        (varModule, vars, register)
          | isVarsTask task = (Register <$> map fst (objToVar task.tmodule.params), [], Nothing)
          | otherwise = ([], task.vars, taskRegister)

isVarsTask :: Task -> Bool
isVarsTask task = task.tmodule.name `elem` ["include_vars", "set_facts"]

fixupArgs :: Task -> Task
fixupArgs task = task {tmodule}
  where
    tmodule = task.tmodule {params}
    params = Object . Data.Aeson.KeyMap.fromList $ case task.tmodule.params of
      v@(String _) -> [("_raw_params", v)]
      (Object obj) -> Data.Aeson.KeyMap.toList obj
      v -> error $ "Unexpected attributes: " <> show v

resolveHostPlay :: FilePath -> HostPlay -> IO HostPlay
resolveHostPlay playSource hostPlay = do
  tasks <-
    annotateDependency . map fixupArgs . concat <$> traverse (resolveImport playSource []) hostPlay.tasks
  pure $ hostPlay {tasks}
  where
    resolveImport :: FilePath -> [FilePath] -> Task -> IO [Task]
    resolveImport source history task = case task.tmodule.name of
      "include_role" -> do
        let role_name = unpack $ fromMaybe "missing name" $ preview (key "name" . _String) $ task.tmodule.params
            role_path = takeDirectory source </> "roles" </> role_name </> "tasks" </> "main.yaml"
            role_default = takeDirectory source </> "roles" </> role_name </> "defaults" </> "main.yaml"
            new_history = checkHistory role_path history
        when (task.loop /= Null) $ error "Loop import is not supported"
        roleDefaults <- objToVar <$> decodeFile role_default
        newTasks <- decodeFile @[Task] role_path
        map (addVars (task.vars <> roleDefaults)) . concat <$> traverse (resolveImport role_path new_history) newTasks
      "include_tasks" -> do
        let task_name = unpack $ fromMaybe "missing name" $ preview _String $ task.tmodule.params
            task_path = takeDirectory source </> task_name
            new_history = checkHistory task_path history
        when (task.loop /= Null) $ error "Loop include is not supported"
        newTasks <- decodeFile @[Task] task_path
        map (addVars task.vars) . concat <$> traverse (resolveImport task_path new_history) newTasks
      "include_vars" -> do
        let params = (fixupArgs task).tmodule.params
            file_nameM = preview (key "file" . _String) params <|> preview (key "_raw_params" . _String) params
            file_name = unpack $ fromMaybe "missing file" file_nameM
            file_path = takeDirectory source </> ".." </> "vars" </> file_name
        when (task.loop /= Null) $ error "Loop include_vars is not supported"
        includedVars <- decodeFile file_path
        -- include_vars is evaluated statically, here we replace the action params with the actual vars.
        let tmodule = Module task.tmodule.name $ case preview (key "name" . _String) params of
              Just n -> Object $ Data.Aeson.KeyMap.fromList [(Data.Aeson.Key.fromText n, includedVars)]
              Nothing -> includedVars
        pure [task {tmodule}]
      _ -> pure [task]
    checkHistory :: FilePath -> [FilePath] -> [FilePath]
    checkHistory fp history
      | fp `elem` history = error $ "Cyclic import detected: " <> show history
      | otherwise = fp : history
    addVars :: [(Text, Value)] -> Task -> Task
    addVars kv task = task {vars = task.vars <> kv}

renderScript :: FilePath -> Playbook -> Text
renderScript inventory (Playbook plays) =
  Text.unlines $
    [ "-- Generated with haxible",
      "{-# LANGUAGE QuasiQuotes, ApplicativeDo, OverloadedStrings #-}",
      "{- cabal:",
      "build-depends: base, haxible",
      "ghc-options: -threaded -rtsopts -with-rtsopts=-N -with-rtsopts=-T",
      "-}",
      "module Main (main) where\n",
      "import Haxible.Eval\n",
      "main :: IO ()",
      "main = runHaxible " <> quote (pack inventory) <> " playbook\n",
      "playbook :: AnsibleHaxl [Value]",
      "playbook = do"
    ]
      <> ((\(idx, play) -> "  " <> playVar idx <> " <- play_" <> play.hosts) <$> indexedPlays)
      <> ["  pure $ " <> Text.intercalate " <> " ((\(idx, _) -> playVar idx) <$> indexedPlays), ""]
      <> concatMap renderPlay plays
  where
    indexedPlays = zip [(0 :: Int) ..] plays
    playVar idx = "_play" <> Text.pack (show idx)
    renderPlay :: HostPlay -> [Text]
    renderPlay play =
      [ "play_" <> play.hosts <> " :: AnsibleHaxl [Value]",
        "play_" <> play.hosts <> " = do"
      ]
        <> ["  let playAttr = " <> textList (mkJsonArg <$> play.playAttrs)]
        <> (mappend "  " <$> map snd tasks)
        <> ["  pure " <> textList (mapMaybe fst tasks), ""]
      where
        tasks = renderCode play.hostVars <$> (zip [0 ..] play.tasks)

quote :: Text -> Text
quote = Text.cons '"' . flip Text.snoc '"'

mkAttributes :: Value -> Text
mkAttributes v = "[json| " <> decodeUtf8 (Data.ByteString.toStrict $ Data.Aeson.encode v) <> " |]"

mkJsonArg :: (Text, Value) -> Text
mkJsonArg (n, v) = "(" <> quote n <> ", " <> mkAttributes v <> ")"

textList :: [Text] -> Text
textList xs = "[" <> Text.intercalate ", " xs <> "]"

textTuple :: [Text] -> Text
textTuple xs = "(" <> Text.intercalate ", " xs <> ")"

renderCode :: [(Text, Value)] -> (Int, Task) -> (Maybe Text, Text)
renderCode globalEnv (idx, task)
  | isVarsTask task = (Nothing, letTask)
  | otherwise = (Just bindVar, Text.unwords (registerBind : taskCall))
  where
    (varNames, varValues) = unzip $ objToVar task.tmodule.params
    letTask = "let " <> textTuple varNames <> " = " <> textTuple (mkAttributes <$> varValues)
    bindVar = case task.register of
      Just v -> v
      Nothing -> "_var" <> Text.pack (show idx)
    registerBind = bindVar <> " <-"

    taskCall = case task.loop of
      Null -> directCall
      Array xs -> mkTraverse $ textList (mkAttributes <$> toList xs)
      String v -> mkTraverse $ "envLoop " <> v <> " " <> envArg
      _ -> error $ "Invalid task.loop: " <> show task.loop

    mkTraverse arg = ["traverse", "(\\item -> "] <> directCall <> [") ", arg]

    mkTaskObject = Object . Data.Aeson.KeyMap.fromList . map (first Data.Aeson.Key.fromText)

    directCall =
      [ "runTask playAttr",
        mkAttributes $ mkTaskObject $ [(task.tmodule.name, task.tmodule.params)] <> task.taskAttrs,
        envArg
      ]

    envArg = "[" <> Text.intercalate ", " env <> "]"
    env = (mkJsonArg <$> (task.vars <> globalEnv)) <> (mkArg <$> (loopVar <> (dependencyVar <$> task.requires)))

    loopVar = case task.loop of
      Null -> []
      _ -> ["item"]

    mkArg v = "(" <> quote v <> ", " <> v <> ")"
