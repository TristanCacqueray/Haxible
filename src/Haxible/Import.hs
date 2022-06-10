-- | This module contains the logic to resolve roles and include tasks
module Haxible.Import
  ( resolveImport,
    BasePlay (..),
    BaseTask (..),
    Play,
    Task,
    TaskValue (..),
    RoleValue (..),
    BlockValue (..),
  )
where

import Haxible.Prelude
import Haxible.Syntax

type Importer a = ReaderT Env IO a

data Env = Env
  { source :: FilePath,
    playSource :: FilePath,
    history :: [FilePath]
  }
  deriving (Eq, Show)

type Play = BasePlay Task

type Task = BaseTask TaskValue

data TaskValue
  = Module (Maybe Text) Value
  | Handler Text Value
  | Role RoleValue
  | Tasks FilePath Text [Task]
  | Facts Vars
  | CacheableFacts Value Vars
  | Block BlockValue
  deriving (Eq, Show)

data RoleValue = RoleValue
  { tasks :: [Task],
    handlers :: [Task],
    defaults :: [(Text, Value)],
    rolePath :: FilePath,
    name :: Text
  }
  deriving (Eq, Show)

data BlockValue = BlockValue
  { tasks :: [Task],
    rescues :: [Task]
  }
  deriving (Eq, Show)

resolveTask :: FilePath -> TaskSyntax -> Importer Task
resolveTask basePath task = do
  templateContent <- case task.module_ of
    "template" -> do
      txt <- readTemplate (fromMaybe (error "template missing src") (preview (key "src" . _String) task.params))
      pure $ Just (from txt)
    _ -> pure Nothing

  taskValue <- case task.module_ of
    "include_role" -> includeRole
    "include_tasks" -> includeTasks
    "set_fact" -> setFact
    "block" -> block
    x
      | x `elem` notImplemented -> do
          source <- asks source
          error $ source <> ": " <> from x <> ": NotImplemented"
      | otherwise -> pure $ Module templateContent task.params

  pure $ task {params = taskValue}
  where
    notImplemented = ["add_host", "import_playbook", "import_role", "meta"]
    withFile fp go = do
      hist <- asks history
      when (fp `elem` hist) $
        error $ "Cyclic import detected: " <> show fp <> " already in " <> show hist
      r <- decodeFile fp
      local (\e -> e {source = fp, history = fp : hist}) $ go r

    getRolePath name path = do
      source <- asks playSource
      pure $ takeDirectory source </> "roles" </> name </> path
    includeRole = do
      let role_name = from $ fromMaybe "missing name" $ preview (key "name" . _String) $ task.params
          name = from role_name
          task_name = from $ fromMaybe "main" $ preview (key "tasks_from" . _String) $ task.params
      role_tasks <- getRolePath role_name $ "tasks" </> task_name <> ".yaml"
      role_defaults <- getRolePath role_name $ "defaults" </> "main.yaml"
      JsonVars defaults <- liftIO do
        defaultExist <- doesFileExist role_defaults
        if defaultExist then decodeFile role_defaults else pure (JsonVars [])
      rolePath <- getRolePath role_name ""
      handlers <- do
        handler_path <- getRolePath role_name $ "handlers" </> "main.yaml"
        handlerExist <- liftIO $ doesFileExist handler_path
        if handlerExist then map resolveHandler <$> decodeFile handler_path else pure []
      withFile role_tasks $ \baseTasks -> do
        tasks <- traverse (resolveTask rolePath) baseTasks
        pure $ Role RoleValue {name, handlers, rolePath, tasks, defaults}

    includeTasks = do
      source <- asks source
      let task_name = from $ fromMaybe "missing name" $ preview _String $ task.params
          task_path = takeDirectory source </> task_name
          task_dir = takeDirectory task_path
      withFile task_path $ \baseTasks -> do
        Tasks task_dir (from task_name) <$> traverse (resolveTask task_dir) baseTasks

    block = do
      tasks <- resolveBlock task.params
      rescues <- resolveBlock (fromMaybe Null (lookup "rescue" task.attrs))
      pure $ Block BlockValue {tasks, rescues}
    resolveBlock :: Value -> Importer [Task]
    resolveBlock = \case
      Null -> pure []
      v -> traverse (resolveTask basePath) . unwrapJSON . fromJSON $ v
    unwrapJSON = \case
      Error e -> error $ "Unexpected json: " <> e
      Success a -> a

    readTemplate (from -> name) = do
      let templatePath = basePath </> "templates" </> name
      liftIO (readFile templatePath)

    setFact = do
      let JsonVars vars = unwrapJSON . fromJSON $ task.params
      pure $ case lookup "cacheable" vars of
        Just v -> CacheableFacts v (filter (\var -> fst var /= "cacheable") vars)
        Nothing -> Facts vars

resolveHandler :: TaskSyntax -> Task
resolveHandler task = task {params = Handler name task.params}
  where
    name =
      fromMaybe
        (error $ "Handler needs a name or a listen: " <> show task)
        (getName "listen" <|> getName "name")
    getName k = preview _String =<< lookup k task.attrs

-- | Transform a 'PlaySyntax' into a resolved 'Play'
resolveImport :: FilePath -> PlaySyntax -> IO Play
resolveImport source (BasePlay baseTasks baseHandlers _ attrs) = do
  let playPath = takeDirectory source
  tasks <- runReaderT (traverse (resolveTask playPath) baseTasks) (Env source source [])
  let handlers = resolveHandler <$> baseHandlers
  pure $ BasePlay {tasks, handlers, playPath, attrs}
