-- | This module is the core of Haxible.
module Haxible.Normalize
  ( normalizePlaybook,
    Definition (..),
    Expr (..),
    HandlerExpr (..),
    Term (..),
    Origin (..),
    Requirement (..),
    Environment (..),
  )
where

import Data.Char qualified
import Data.Map qualified as Map
import Data.Text qualified as Text
import Haxible.Import
import Haxible.Prelude
import Haxible.Syntax (propagableAttrs)

-- $setup
-- >>> let mkTask attrs = BaseTask {name = Nothing, module_ = "", params = Module Nothing "", attrs}
-- >>> let mkRes name = Resource (Binder name) (Register name)

-- | A definition is like a function, to represent a play, a role, or a list of tasks.
data Definition = Definition
  { name :: Text,
    outputs :: Environment,
    playAttrs :: Vars,
    -- | roles defaults vars, that are added to the lowest priority
    defaultVars :: Vars,
    -- | The location of the tasks, to enable loading adjacent python module in a `library` folder.
    source :: FilePath,
    exprs :: [Expr],
    handlers :: [HandlerExpr]
  }
  deriving (Show, Eq)

data HandlerExpr = HandlerExpr {listen :: Text, module_ :: Text, handlerAttrs :: Vars} deriving (Show, Eq)

-- | An expression is a single instruction.
data Expr = Expr
  { binder :: Binder,
    requires :: [Resource],
    provides :: [Resource],
    outputs :: Either Environment [Resource],
    inputs :: [Requirement],
    when_ :: Maybe Value,
    loop :: Maybe Value,
    taskAttrs :: Vars,
    term :: Term
  }
  deriving (Show, Eq)

-- | A term is the expression value.
data Term
  = ModuleCall {module_ :: Text, params :: Value}
  | DefinitionCall {defName :: Text, taskVars :: Vars, rescue :: Bool}
  deriving (Show, Eq)

-- | A resource is a global object such as a registered result or a file path.
data Resource = Resource {name :: Binder, dep :: Dependency} deriving (Eq, Show)

data Dependency
  = Register Text
  | Path Text
  | -- Command module in the same path creates dependency
    Command FilePath
  | IncludedVars FilePath
  deriving (Eq, Show)

-- | A binder is a haskell variable name.
newtype Binder = Binder Text deriving (Eq, Show)

instance From Binder Text where from (Binder b) = b

-- | A binder can match multiple resources, for example:
--
--   - stat:
--       path: /etc/zuul
--     register: etc_zuul
--
--   -> the binder matches (Register "etc_zuul") and (Path "/etc/zuul").
--
-- A definition produces a list of binder too, for example:
--
--   - include_role:
--       name: adder
--
--   -> the binder matches each task
newtype Environment = Environment {getEnv :: [(Binder, Either Environment [Resource])]}
  deriving (Show, Eq)

envCount :: Environment -> Int
envCount (Environment e) = foldl' (+) 0 $ map (getCount . snd) e
  where
    getCount = \case
      Left x -> envCount x
      Right _ -> 1

dependencyValue, dependencyName :: Dependency -> Text
dependencyName = \case
  Register n -> n
  Path p ->
    -- a fake variable that is used to force the dependency relationship
    "_fake_" <> cleanName p
  Command fp -> "_fake_" <> cleanName (from fp)
  IncludedVars fp -> "_fakev_" <> cleanName (from fp)
dependencyValue = \case
  Register n -> n
  Path p -> p
  Command _ -> "__haxible_command"
  IncludedVars _ -> "__haxible_included_vars"

-- | The requirements indicates what binders are used by an expression.
data Requirement = Requirement {name :: Text, origin :: Origin} deriving (Show, Eq)

data Origin = Direct Binder | LoopVar | Nested Binder Int deriving (Show, Eq)

data Env = Env
  { names :: [Text],
    definitions :: [Definition],
    availables :: [Resource]
  }
  deriving (Generic)

emptyEnv :: Env
emptyEnv = Env [] [] []

type ReqAcc = ([(Binder, [Resource])], [(Binder, Environment)])

-- | propagate binders to sub expression to set the inputs.
solveInputs :: [Definition] -> [Definition]
solveInputs defs = map updateCallEnv defs
  where
    updateCallEnv :: Definition -> Definition
    updateCallEnv def = def {exprs = reverse . fst . foldl' setCallEnv ([], ([], [])) $ def.exprs}

    outputs = Map.fromList ((\def -> (def.name, def.outputs)) <$> defs)
    getOutputs name = fromMaybe (error $ from name <> ": Unknown def?!") (Map.lookup name outputs)

    setCallEnv :: ([Expr], ReqAcc) -> Expr -> ([Expr], ReqAcc)
    setCallEnv (acc, (avail, nested)) expr = (newExpr : acc, (newAvail, newNested))
      where
        (newAvail, newNested) = case expr.term of
          ModuleCall {} -> ((expr.binder, expr.provides) : avail, nested)
          DefinitionCall {rescue, defName} ->
            let name
                  -- If the call has a rescue block, assume only the failed branch is available
                  | rescue = defName <> "Rescue"
                  | otherwise = defName
             in (avail, (expr.binder, getOutputs name) : nested)
        newExpr = expr {inputs = expr.inputs <> directRequirement <> nestedRequirement}

        reMatch :: [Resource] -> Bool
        reMatch = any (`elem` expr.requires)

        -- Look for available requirement in the direct binders (e.g. from task register)
        directRequirement :: [Requirement]
        directRequirement = concatMap (uncurry toReq) $ filter (reMatch . snd) avail
          where
            toReq :: Binder -> [Resource] -> [Requirement]
            toReq name = map (\x -> Requirement {name = dependencyName x.dep, origin = Direct name})

        -- Look for available requirement in nested binders (e.g. from other play)
        nestedRequirement :: [Requirement]
        nestedRequirement = concatMap (uncurry (go 0)) nested
          where
            go :: Int -> Binder -> Environment -> [Requirement]
            go binderPos binder =
              concatMap (\(envPos, binderResources) -> toReq envPos binder binderResources)
                . setStartingPos binderPos
                . map snd
                . getEnv
            setStartingPos :: Int -> [Either Environment [Resource]] -> [(Int, Either Environment [Resource])]
            setStartingPos startingPos = reverse . snd . foldl' goSet (startingPos, [])
              where
                goSet :: (Int, [(Int, Either Environment [Resource])]) -> Either Environment [Resource] -> (Int, [(Int, Either Environment [Resource])])
                goSet (curPos, acc') x = case x of
                  Right _ -> (curPos + 1, (curPos, x) : acc')
                  Left e -> (curPos + envCount e, (curPos, x) : acc')

            toReq :: Int -> Binder -> Either Environment [Resource] -> [Requirement]
            toReq resourcePos binder = \case
              Right resources
                | reMatch resources -> concatMap (toNestedReq binder) (zip [resourcePos ..] resources)
                | otherwise -> []
              Left env -> go resourcePos binder env
            toNestedReq :: Binder -> (Int, Resource) -> [Requirement]
            toNestedReq binder (pos, res)
              | res `elem` expr.requires =
                  [Requirement {name = dependencyName res.dep, origin = Nested binder pos}]
              | otherwise = []

-- | Create a unique name:
-- >>> evalState (traverse (freshName "play") ["host", "host"]) emptyEnv
-- [Binder "playHost0",Binder "playHost1"]
freshName :: Text -> Text -> State Env Binder
freshName base identifier = do
  names <- gets names
  let name = base <> cleanName identifier
      newName = mappend name $ head $ filter isFresh $ map (from . show @Int) [0 ..]
      isFresh x = name <> x `notElem` names
  modify (\env -> env {names = newName : names})
  pure $ Binder newName

-- | Convert to ascii title
-- >>> cleanName <$> ["create host", "start:network"]
-- ["CreateHost","StartNetwork"]
cleanName :: Text -> Text
cleanName = mconcat . map Text.toTitle . Text.split (not . Data.Char.isAlphaNum)

-- | Extract requirements from a list of value
--
-- >>> getRequires (mkRes <$> ["hostname", "file_stat"]) [[json|{"ping": "{{ hostname }}"}|]]
-- [Resource {name = Binder "hostname", dep = Register "hostname"}]
getRequires :: [Resource] -> [Value] -> [Resource]
getRequires availables = concatMap findRequirements
  where
    findRequirements :: Value -> [Resource]
    findRequirements v = case v of
      String x -> case filter (\n -> dependencyValue (n.dep) `Text.isInfixOf` x) availables of
        [] -> []
        requirement -> requirement
      Object x -> concatMap findRequirements x
      Array x -> concatMap findRequirements x
      _ -> []

-- | Discover the requires/provides and create the base expr.
baseExpr :: FilePath -> Binder -> Task -> [Value] -> [Dependency] -> Term -> State Env Expr
baseExpr taskPath binder task reqValues baseProvides term = do
  availables <- gets availables

  -- Look for requirements and provides
  let moduleRequires = getRequires availables (fromMaybe Null loop : fromMaybe Null when_ : reqValues)
      moduleProvides = Resource binder <$> baseProvides
  -- Command defined in the same block/tasks/role are automatically inter-dependent.
  let commandRequires
        | task.module_ == "command" = filter (samePathCommand taskPath . (.dep)) availables
        | otherwise = []
      commandProvides
        | task.module_ == "command" = [Resource binder (Command taskPath)]
        | otherwise = []
  -- Included vars in the same block/tasks/role are automatically required
  let includedVarsRequires = filter (samePathVars taskPath . (.dep)) availables

  let requires = includedVarsRequires <> commandRequires <> moduleRequires
      provides = commandProvides <> moduleProvides
  modify (\env -> env {availables = provides <> availables})

  -- Create the expr
  let outputs = Right provides
  pure $ Expr {binder, requires, provides, outputs, inputs, loop, term, taskAttrs, when_}
  where
    samePathCommand fp = \case
      Command fp' | fp == fp' -> True
      _ -> False
    samePathVars fp = \case
      IncludedVars fp' | fp == fp' -> True
      _ -> False

    (loop, inputs) = case lookup "loop" task.attrs of
      Just v ->
        let loopVar = fromMaybe "item" (getLoopVar =<< lookup "loop_control" task.attrs)
         in (Just v, [Requirement loopVar LoopVar])
      Nothing -> (Nothing, [])
    getLoopVar = preview (key "loop_var" . _String)

    when_ = lookup "when" task.attrs
    taskAttrs = (filter ((==) "vars" . fst) task.attrs) <> getPropagableAttrs task.attrs

moduleExpr :: FilePath -> Task -> Maybe Text -> Value -> State Env Expr
moduleExpr taskPath task template value = do
  binder <- freshName task.module_ (fromMaybe "" task.name)
  baseExpr taskPath binder task (value : templateAttr : attrs) (maybeToList register <> maybeToList destPath) term
  where
    term = ModuleCall {module_ = task.module_, params = value}
    destPath = Path <$> (getAttr "path" <|> getAttr "dest")
    register = Register <$> (preview _String =<< lookup "register" task.attrs)
    getAttr n = preview (key n . _String) value
    attrs = fromMaybe Null . flip lookup task.attrs <$> ("vars" : propagableAttrs)
    templateAttr = maybe Null String template

getPropagableAttrs :: Vars -> Vars
getPropagableAttrs = filter (`elemFst` ("name" : propagableAttrs))
  where
    elemFst = elem . fst

definitionExpr :: Definition -> Maybe Definition -> Task -> State Env Expr
definitionExpr def rescueDef task = do
  when (isJust $ lookup "register" task.attrs) (error $ "Register definition call is not supported for: " <> show task)

  -- Add the definition to the environment
  addDefinition def
  case rescueDef of
    Just def' -> addDefinition def'
    Nothing -> pure ()

  -- Create the definition call expression
  expr <- baseExpr "" binder task vars provides term
  let requires = expr.requires <> defRequires
  pure $ expr {outputs, requires, taskAttrs}
  where
    addDefinition newDef = do
      modify (#definitions %~ (newDef {exprs = propagateTaskAttrs <$> newDef.exprs} :))
    propagateTaskAttrs :: Expr -> Expr
    propagateTaskAttrs e = e {taskAttrs = e.taskAttrs <> (filter (\(k, _) -> k `elem` propagableAttrs) task.attrs)}

    defRequires =
      concatMap (.requires) def.exprs <> case rescueDef of
        Just def' -> concatMap (.requires) def'.exprs
        Nothing -> []

    binder = Binder $ "results" <> Text.toUpper (Text.take 1 defName) <> Text.drop 1 defName
    vars = map snd def.defaultVars
    provides = concatMap (\e -> map (.dep) e.provides) def.exprs
    term = DefinitionCall {defName, taskVars, rescue = isJust rescueDef}
    outputs = Left def.outputs
    defName
      | isJust rescueDef = Text.dropEnd 4 def.name
      | otherwise = def.name
    taskAttrs = getTaskAttrs task
    taskVars = getTaskVars task

includeRoleExpr :: Task -> RoleValue -> State Env Expr
includeRoleExpr task role = do
  Binder defName <- freshName "role" role.name
  roleDef <- normalizeDefinitionWithHandlers role.handlers role.rolePath defName role.tasks
  definitionExpr (roleDef {defaultVars = role.defaults}) Nothing task

includeTasksExpr :: FilePath -> Task -> Text -> [Task] -> State Env Expr
includeTasksExpr tasksPath task includeName tasks = do
  Binder defName <- freshName "tasks" includeName
  tasksDef <- normalizeDefinition tasksPath defName tasks
  definitionExpr tasksDef Nothing task

blockExpr :: FilePath -> Task -> BlockValue -> State Env Expr
blockExpr parentPath task block = do
  Binder defName <- freshName "block" (fromMaybe "" task.name)
  case block.rescues of
    [] -> do
      blockDef <- normalizeDefinition parentPath defName block.tasks
      definitionExpr blockDef Nothing task
    _ -> do
      blockDef <- normalizeDefinition parentPath (defName <> "Main") block.tasks
      rescueDef <- normalizeDefinition parentPath (defName <> "Rescue") block.rescues
      definitionExpr blockDef (Just rescueDef) task

factsExpr :: Task -> Maybe Value -> Text -> Value -> State Env Expr
factsExpr task cacheable name value = do
  when (isJust (lookup "loop" task.attrs)) $ error "set_fact loop is not supported"
  binder <- freshName "facts" (fromMaybe "" task.name)
  baseExpr "" binder task [value] [Register name] term
  where
    term = ModuleCall {module_ = "set_fact", params}
    params = mkObj $ [(name, value)] <> maybe [] (\v -> [("cacheable", v)]) cacheable

includeVarsExpr :: FilePath -> Task -> Value -> State Env Expr
includeVarsExpr taskPath task value = do
  binder <- freshName "vars" (fromMaybe "" task.name)
  expr <- baseExpr taskPath binder task [value] [dep] term
  let outputs = Right $ case dep of
        -- Don't propagate locally included vars to the caller
        IncludedVars _ -> []
        _ -> expr.provides
  pure $ expr {outputs, term}
  where
    term = ModuleCall {module_ = "include_vars", params = value}
    dep = case preview (key "name" . _String) value of
      -- If the include_vars has a name attribute, then it's just a regular register
      Just n -> Register n
      -- Otherwise, the task needs a special case to propagate each individual vars.
      Nothing -> IncludedVars taskPath

normalizeTask :: FilePath -> Task -> State Env [Expr]
normalizeTask taskPath task =
  case task.params of
    Module t v -> (: []) <$> moduleExpr taskPath task t v
    Role r -> (: []) <$> includeRoleExpr task r
    Tasks tasksPath name xs -> (: []) <$> includeTasksExpr tasksPath task name xs
    Facts vars -> traverse (uncurry (factsExpr task Nothing)) vars
    CacheableFacts cacheable vars -> traverse (uncurry (factsExpr task (Just cacheable))) vars
    IncludeVars v -> (: []) <$> includeVarsExpr taskPath task v
    Block bv -> (: []) <$> blockExpr taskPath task bv
    Handler {} -> error "The impossible has happened: handler can't be in task list"

normalizeDefinitionWithHandlers :: [Task] -> FilePath -> Text -> [Task] -> State Env Definition
normalizeDefinitionWithHandlers handlersTask source name tasks = do
  modify (\env -> env {availables = updateAvailable env.availables})
  exprs <- concat <$> traverse (normalizeTask source) tasks
  let outputs = Environment $ (\e -> (e.binder, e.outputs)) <$> exprs
      handlers = mkHandlers <$> handlersTask
  pure $ Definition {name, exprs, handlers, outputs, playAttrs = [], defaultVars = [], source}
  where
    mkHandlers task = case task.params of
      Handler listen params ->
        let handlerAttrs = [(task.module_, params)] <> filter ((/=) "listen" . fst) task.attrs
         in HandlerExpr {listen, module_ = task.module_, handlerAttrs}
      _ -> error "The impossible has happened: non handler can't be in handler list"
    -- Cleanup unrelated resource.
    updateAvailable = filter (not . unusedResource . (.dep))
    unusedResource = \case
      Command fp -> source == fp
      IncludedVars fp -> source == fp
      Register _ -> False
      Path _ -> False

normalizeDefinition :: FilePath -> Text -> [Task] -> State Env Definition
normalizeDefinition = normalizeDefinitionWithHandlers []

normalizePlay :: Play -> State Env Definition
normalizePlay play = do
  Binder name <- freshName "play" playName
  addPlayAttrs <$> normalizeDefinitionWithHandlers play.handlers play.playPath name play.tasks
  where
    addPlayAttrs def = def {playAttrs = play.attrs}
    getAttr name = preview _String =<< lookup name play.attrs
    playName = fromMaybe "" (getAttr "name" <|> getAttr "hosts")

-- | Extract the vars from a task object:
-- >>> getTaskVars (mkTask [("vars", [json|{"test": null}|])])
-- [("test",Null)]
getTaskVars :: Task -> Vars
getTaskVars task = itoListOf members (fromMaybe Null $ lookup "vars" task.attrs)

getTaskAttrs :: Task -> Vars
getTaskAttrs task = filter (\(k, _) -> k `elem` propagableAttrs) task.attrs

-- | Transform a list of 'Play' into a list of 'Definition'.
normalizePlaybook :: [Play] -> [Definition]
normalizePlaybook plays =
  let (xs, env) = flip runState emptyEnv do
        defs <- traverse normalizePlay plays
        exprs <- traverse topLevelCall defs
        pure $ topLevel exprs : defs
   in solveInputs (xs <> env.definitions)
  where
    topLevelCall def = do
      let binder = Binder ("results" <> Text.toUpper (Text.take 1 def.name) <> Text.drop 1 def.name)
          term = DefinitionCall {defName = def.name, taskVars = [], rescue = False}
          outputs = Right []
          when_ = Nothing
          taskAttrs = []
          provides = nub $ concatMap (.provides) def.exprs
          requires = nub $ filter (`notElem` provides) $ concatMap (.requires) def.exprs
      pure $ Expr {binder, requires, provides, outputs, inputs = [], loop = Nothing, term, taskAttrs, when_}
    topLevel :: [Expr] -> Definition
    topLevel exprs = Definition "playbook" (Environment []) [] [] "" exprs []
