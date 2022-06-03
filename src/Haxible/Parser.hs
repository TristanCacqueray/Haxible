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
import Data.Text (Text, unpack)
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

annotateDependency :: [Task] -> [Task]
annotateDependency = reverse . fst . foldl' go ([], [])
  where
    go :: ([Task], [(Text, Text)]) -> Task -> ([Task], [(Text, Text)])
    go (xs, available) task = (task {requires, register} : xs, newAvailable)
      where
        -- look in all string value to see if an available is used.
        findRequirements :: Value -> [Text]
        findRequirements v = case v of
          String x -> case filter (\(_, n) -> n `Text.isInfixOf` x) available of
            [] -> []
            requirement -> map fst requirement
          Object x -> concatMap findRequirements x
          Array x -> concatMap findRequirements x
          _ -> []

        requires = concatMap findRequirements (task.loop : task.attributes : map snd task.vars)
        register =
          -- If there is no register but a file destination, then add a fake register
          task.register <|> case dest of
            [(r, _)] -> Just r
            _ -> Nothing

        newAvailable = reg <> dest <> available
        reg = case task.register of
          Just n -> [(n, n)]
          Nothing -> []

        getAttr n = preview (key n . _String) task.attributes
        dest = case getAttr "path" <|> getAttr "dest" of
          Just n -> case task.register of
            -- re-use the register named
            Just r -> [(r, n)]
            -- add a fake register value
            Nothing -> [(Text.replace "/" "_" $ Text.replace "." "_" n, n)]
          Nothing -> []

fixupArgs :: Task -> Task
fixupArgs task = task {attributes}
  where
    attributes = Object (Data.Aeson.KeyMap.fromList $ args <> params)
    params = []
    args :: [(Data.Aeson.Key.Key, Value)]
    args = case task.attributes of
      String _ -> [("_raw_params", task.attributes)]
      Object obj -> Data.Aeson.KeyMap.toList obj
      v -> error $ "Unexpected attributes: " <> show v

resolveHostPlay :: FilePath -> HostPlay -> IO HostPlay
resolveHostPlay source hostPlay = do
  tasks <-
    annotateDependency . map fixupArgs . concat <$> traverse (resolveImport []) hostPlay.tasks
  pure $ hostPlay {tasks}
  where
    resolveImport :: [FilePath] -> Task -> IO [Task]
    resolveImport history task = case task.action of
      "include_role" -> do
        let role_name = unpack $ fromMaybe "missing name" $ preview (key "name" . _String) $ task.attributes
            role_path = takeDirectory source </> "roles" </> role_name </> "tasks" </> "main.yaml"
            role_default = takeDirectory source </> "roles" </> role_name </> "defaults" </> "main.yaml"
            new_history = checkHistory role_path history
        when (task.loop /= Null) $ error "Loop import is not supported"
        roleDefaults <- objToVar <$> decodeFile role_default
        newTasks <- decodeFile @[Task] role_path
        map (addVars (task.vars <> roleDefaults)) . concat <$> traverse (resolveImport new_history) newTasks
      "include_tasks" -> do
        let task_name = unpack $ fromMaybe "missing name" $ preview _String task.attributes
            task_path = takeDirectory source </> task_name
            new_history = checkHistory task_path history
        when (task.loop /= Null) $ error "Loop include is not supported"
        newTasks <- decodeFile @[Task] task_path
        map (addVars task.vars) . concat <$> traverse (resolveImport new_history) newTasks
      _ -> pure [task]
    checkHistory :: FilePath -> [FilePath] -> [FilePath]
    checkHistory fp history
      | fp `elem` history = error $ "Cyclic import detected: " <> show history
      | otherwise = fp : history
    addVars :: [(Text, Value)] -> Task -> Task
    addVars kv task = task {vars = task.vars <> kv}

renderScript :: Playbook -> Text
renderScript (Playbook plays) =
  Text.unlines $
    [ "{-# LANGUAGE QuasiQuotes, ApplicativeDo, OverloadedStrings #-}",
      "{- cabal:",
      "build-depends: base, haxible",
      "ghc-options: -threaded -rtsopts -with-rtsopts=-N -with-rtsopts=-T",
      "-}",
      "module Main (main) where\n",
      "import Haxible.Eval\n",
      "main :: IO ()",
      "main = runHaxible playbook\n",
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
        <> ["  pure " <> textList (map fst tasks), ""]
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

renderCode :: [(Text, Value)] -> (Int, Task) -> (Text, Text)
renderCode globalEnv (idx, task) = (bindVar, Text.unwords (registerBind : taskCall))
  where
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

    directCall =
      [ "runTask playAttr",
        "(" <> Text.pack (show task.name) <> ")",
        quote task.action,
        mkAttributes task.attributes,
        envArg
      ]

    envArg = "[" <> Text.intercalate ", " env <> "]"
    env = (mkJsonArg <$> (task.vars <> globalEnv)) <> (mkArg <$> (loopVar <> task.requires))

    loopVar = case task.loop of
      Null -> []
      _ -> ["item"]

    mkArg v = "(" <> quote v <> ", " <> v <> ")"
