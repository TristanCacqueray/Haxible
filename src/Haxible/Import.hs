-- | This module contains the logic to resolve roles and include tasks
module Haxible.Import
  ( resolveImport,
    BasePlay (..),
    BaseTask (..),
    Play,
    Task,
    TaskValue (..),
    RoleValue (..),
  )
where

import Haxible.Prelude
import Haxible.Syntax

type Importer a = ReaderT Env IO a

data Env = Env
  { source :: FilePath,
    history :: [FilePath]
  }
  deriving (Eq, Show)

type Play = BasePlay Task

type Task = BaseTask TaskValue

data TaskValue
  = Module Value
  | Role RoleValue
  | Tasks Text [Task]
  | Facts Vars
  | CacheableFacts Value Vars
  | Block BlockValue
  deriving (Eq, Show)

data RoleValue = RoleValue
  { tasks :: [Task],
    defaults :: [(Text, Value)],
    name :: Text
  }
  deriving (Eq, Show)

data BlockValue = BlockValue
  { tasks :: [Task],
    rescues :: [Task]
  }
  deriving (Eq, Show)

resolveTask :: TaskSyntax -> Importer Task
resolveTask task = do
  taskValue <- case task.module_ of
    "include_role" -> includeRole
    "include_tasks" -> includeTasks
    "set_fact" -> setFact
    "block" -> block
    _ -> pure $ Module task.params
  pure $ task {params = taskValue}
  where
    withFile fp go = do
      hist <- asks history
      when (fp `elem` hist) $
        error $ "Cyclic import detected: " <> show fp <> " already in " <> show hist
      r <- decodeFile fp
      local (const $ Env fp (fp : hist)) $ go r

    getRolePath name path = do
      source <- asks source
      pure $ takeDirectory source </> "roles" </> name </> path
    includeRole = do
      let role_name = from $ fromMaybe "missing name" $ preview (key "name" . _String) $ task.params
          name = from role_name
      role_path <- getRolePath role_name $ "tasks" </> "main.yaml"
      role_defaults <- getRolePath role_name $ "defaults" </> "main.yaml"
      JsonVars defaults <- decodeFile role_defaults
      withFile role_path $ \baseTasks -> do
        tasks <- traverse resolveTask baseTasks
        pure $ Role RoleValue {name, tasks, defaults}

    includeTasks = do
      source <- asks source
      let task_name = from $ fromMaybe "missing name" $ preview _String $ task.params
          task_path = takeDirectory source </> task_name
      withFile task_path $ \baseTasks -> do
        Tasks (from task_name) <$> traverse resolveTask baseTasks

    block = do
      tasks <- resolveBlock task.params
      rescues <- resolveBlock (fromMaybe Null (lookup "rescue" task.attrs))
      pure $ Block BlockValue {tasks, rescues}
    resolveBlock :: Value -> Importer [Task]
    resolveBlock = traverse resolveTask . unwrapJSON . fromJSON
    unwrapJSON = \case
      Error e -> error $ "Unexpected json: " <> e
      Success a -> a

    setFact = do
      let JsonVars vars = unwrapJSON . fromJSON $ task.params
      pure $ case lookup "cacheable" vars of
        Just v -> CacheableFacts v (filter (\var -> fst var /= "cacheable") vars)
        Nothing -> Facts vars

-- | Transform a 'PlaySyntax' into a resolved 'Play'
resolveImport :: FilePath -> PlaySyntax -> IO Play
resolveImport source (BasePlay baseTasks attrs) = do
  tasks <- runReaderT (traverse resolveTask baseTasks) (Env source [])
  pure $ BasePlay {tasks, attrs}
