-- | This module is the core of Haxible.
module Haxible.Normalize
  ( normalizePlaybook,
    Definition (..),
    Expr (..),
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
    requires :: [Resource],
    provides :: [Resource],
    outputs :: Environment,
    playAttrs :: Vars,
    -- | roles defaults vars, that are added to the lowest priority
    defaultVars :: Vars,
    -- | The location of the tasks, to enable loading adjacent python module in a `library` folder.
    source :: FilePath,
    exprs :: [Expr],
    handlers :: [(Text, Text, Vars)]
  }
  deriving (Show, Eq)

emptyDefinition :: Text -> FilePath -> Definition
emptyDefinition name source =
  Definition {name, requires = [], provides = [], outputs = Environment [], playAttrs = [], defaultVars = [], exprs = [], handlers = [], source}

-- | An expression is a single instruction.
data Expr = Expr
  { binder :: Binder,
    requires :: [Resource],
    provides :: [Resource],
    outputs :: Either Environment [Resource],
    inputs :: [Requirement],
    when_ :: Maybe Value,
    loop :: Maybe Value,
    rescue :: Bool,
    taskAttrs :: Vars,
    term :: Term
  }
  deriving (Show, Eq)

-- | A term is the expression value.
data Term
  = ModuleCall {module_ :: Text, params :: Value}
  | DefinitionCall {defName :: Text, taskVars :: Vars}
  deriving (Show, Eq)

-- | A resource is a global object such as a registered result or a file path.
data Resource = Resource
  { name :: Binder,
    dep :: Dependency
  }
  deriving (Eq, Show)

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

-- | propagate binders to sub expression to set the inputs
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
          DefinitionCall {defName} ->
            let name
                  | not expr.rescue = defName
                  | otherwise = defName <> "Rescue"
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

        -- Look for available rqeuirement in nested binders (e.g. from other play)
        nestedRequirement :: [Requirement]
        nestedRequirement = concatMap (uncurry (go 0)) nested
          where
            go :: Int -> Binder -> Environment -> [Requirement]
            go binderPos binder =
              concatMap (\(envPos, binderResources) -> toReq (binderPos + envPos) binder binderResources)
                . zip [0 ..]
                . map snd
                . getEnv
            toReq :: Int -> Binder -> Either Environment [Resource] -> [Requirement]
            toReq resourcePos binder = \case
              Right resources
                | reMatch resources -> concatMap (toNestedReq resourcePos binder) (zip [0 ..] resources)
                | otherwise -> []
              Left env -> go resourcePos binder env
            toNestedReq :: Int -> Binder -> (Int, Resource) -> [Requirement]
            toNestedReq resourcePos binder (pos, res)
              | res `elem` expr.requires =
                  [Requirement {name = dependencyName res.dep, origin = Nested binder (resourcePos + pos)}]
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

-- | Extract requirements from a task value
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

samePathCommand :: FilePath -> Dependency -> Bool
samePathCommand fp = \case
  Command fp' | fp == fp' -> True
  _ -> False

samePathVars :: FilePath -> Dependency -> Bool
samePathVars fp = \case
  IncludedVars fp' | fp == fp' -> True
  _ -> False

moduleExpr :: FilePath -> Task -> Maybe Text -> Value -> State Env Expr
moduleExpr taskPath task template value = do
  binder <- freshName task.module_ (fromMaybe "" task.name)

  availables <- gets availables

  -- Look for requirements and provides
  let moduleRequires = getRequires availables (value : templateAttr : attrs)
      moduleProvides = Resource binder <$> maybeToList register <> maybeToList destPath
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
  let term = ModuleCall {module_ = task.module_, params = value}
      inputs = []
      outputs = Right provides
  pure $ Expr {binder, requires, provides, outputs, inputs, loop = Nothing, term, taskAttrs, when_, rescue}
  where
    rescue = False
    when_ = lookup "when" task.attrs
    destPath = Path <$> (getAttr "path" <|> getAttr "dest")
    register = Register <$> (preview _String =<< lookup "register" task.attrs)
    getAttr n = preview (key n . _String) value
    taskAttrs = (filter ((==) "vars" . fst) task.attrs) <> getPropagableAttrs task.attrs
    attrs = fromMaybe Null . flip lookup task.attrs <$> ("vars" : propagableAttrs)
    templateAttr = maybe Null String template

callExpr :: Task -> State Env Expr
callExpr t = moduleExpr "" t Nothing Null

getPropagableAttrs :: Vars -> Vars
getPropagableAttrs = filter (`elemFst` ("name" : propagableAttrs))
  where
    elemFst = elem . fst

roleExpr :: Task -> RoleValue -> State Env Expr
roleExpr task role = do
  when (isJust $ lookup "register" task.attrs) (error "Register include_role is not supported")
  Binder defName <- freshName "role" role.name
  roleDef <- normalizeDefinitionWithHandlers role.handlers role.rolePath defName role.tasks
  modify (#definitions %~ (roleDef {defaultVars = role.defaults} :))

  -- Create a fake task value with role defaults so that requires are looked for in them
  expr <- moduleExpr "" task Nothing (mkObj role.defaults)
  binder <- freshName "results" role.name
  pure $ expr {binder, taskAttrs, term = DefinitionCall {defName, taskVars}}
  where
    taskAttrs = getTaskAttrs task
    taskVars = getTaskVars task

tasksExpr :: FilePath -> Task -> Text -> [Task] -> State Env Expr
tasksExpr tasksPath task includeName tasks = do
  when (isJust $ lookup "register" task.attrs) (error "Register include_tasks is not supported")
  Binder defName <- freshName "tasks" includeName
  tasksDef <- normalizeDefinition tasksPath defName tasks
  modify (#definitions %~ (tasksDef :))

  expr <- callExpr task
  binder <- freshName "results" defName
  let outputs = Left tasksDef.outputs
  pure $ expr {binder, outputs, taskAttrs, term = DefinitionCall {defName, taskVars}}
  where
    taskVars = getTaskVars task
    taskAttrs = getTaskAttrs task

factsExpr :: Task -> Maybe Value -> Text -> Value -> State Env Expr
factsExpr task cacheable name value = do
  -- exprs
  binder <- freshName "facts" (fromMaybe "" task.name)
  availables <- gets availables
  let requires = getRequires availables [value]
      resource = Resource {name = binder, dep = Register name}
      provides = [resource]
      outputs = Right provides
      params = mkObj $ [(name, value)] <> maybe [] (\v -> [("cacheable", v)]) cacheable
      taskAttrs = (filter ((==) "vars" . fst) task.attrs) <> getPropagableAttrs task.attrs
      term = ModuleCall {module_ = "set_fact", params}
      loop = Nothing
      rescue = False
      inputs = []
      when_ = lookup "when" task.attrs
  modify (\env -> env {availables = resource : availables})
  -- expr <- moduleExpr task value
  when (isJust (lookup "loop" task.attrs)) $ error "set_fact loop is not supported"
  pure $ Expr {binder, requires, provides, outputs, inputs, loop, term, taskAttrs, when_, rescue}

includeVarsExpr :: FilePath -> Task -> Value -> State Env Expr
includeVarsExpr taskPath task value = do
  binder <- freshName "vars" (fromMaybe "" task.name)
  expr <- moduleExpr taskPath task Nothing value
  let resource = Resource binder dep
      provides = [resource]
      outputs = case dep of
        -- Don't propagate locally included vars to the caller
        IncludedVars _ -> Right []
        _ -> Right provides
  modify (\env -> env {availables = resource : env.availables})
  pure $ expr {binder, provides, outputs, term}
  where
    term = ModuleCall {module_ = "include_vars", params = value}
    dep = case preview (key "name" . _String) value of
      -- If the include_vars has a name attribute, then it's just a regular register
      Just n -> Register n
      -- Otherwise, we need special care to propagate each individual vars.
      Nothing -> IncludedVars taskPath

blockExpr :: FilePath -> Task -> BlockValue -> State Env Expr
blockExpr parentPath task block = do
  binder <- freshName "block" (fromMaybe "" task.name)
  let defName = from binder

  (outs, rescue) <- case block.rescues of
    [] -> do
      blockDef <- normalizeDefinition parentPath defName block.tasks
      modify (#definitions %~ (blockDef :))
      pure (blockDef.outputs, False)
    _ -> do
      blockDef <- normalizeDefinition parentPath (defName <> "Main") block.tasks
      rescueDef <- normalizeDefinition parentPath (defName <> "Rescue") block.rescues
      modify (#definitions %~ ([rescueDef, blockDef] <>))
      pure (rescueDef.outputs, True)

  expr <- callExpr task
  let outputs = Left outs
      taskVars = getTaskVars task
      taskAttrs = getTaskAttrs task
  pure $ expr {binder, outputs, taskAttrs, rescue, term = DefinitionCall {defName, taskVars}}

normalizeTask :: FilePath -> Task -> State Env [Expr]
normalizeTask taskPath task = do
  exprs <- case task.params of
    Module t v -> (: []) <$> moduleExpr taskPath task t v
    Role r -> (: []) <$> roleExpr task r
    Tasks tasksPath name xs -> (: []) <$> tasksExpr tasksPath task name xs
    Facts vars -> traverse (uncurry (factsExpr task Nothing)) vars
    IncludeVars v -> (: []) <$> includeVarsExpr taskPath task v
    CacheableFacts cacheable vars -> traverse (uncurry (factsExpr task (Just cacheable))) vars
    Block bv -> (: []) <$> blockExpr taskPath task bv
    Handler {} -> error "The impossible has happened: handler can't be in task list"
  pure $ map addLoopReq exprs
  where
    addLoopReq expr = expr {loop, inputs = extraReq <> expr.inputs}
    (loop, extraReq) = case lookup "loop" task.attrs of
      Just v ->
        let loopVar = fromMaybe "item" (getLoopVar =<< lookup "loop_control" task.attrs)
         in (Just v, [Requirement loopVar LoopVar])
      Nothing -> (Nothing, [])
    getLoopVar = preview (key "loop_var" . _String)

normalizeDefinitionWithHandlers :: [Task] -> FilePath -> Text -> [Task] -> State Env Definition
normalizeDefinitionWithHandlers handlersTask source name tasks = do
  modify (\env -> env {availables = updateAvailable env.availables})
  exprs <- concat <$> traverse (normalizeTask source) tasks
  let provides = nub $ concatMap (.provides) exprs
      requires = nub $ filter (`notElem` provides) $ concatMap (.requires) exprs
      outputs = Environment $ (\e -> (e.binder, e.outputs)) <$> exprs
      handlers = mkHandlers <$> handlersTask
  pure $ Definition {name, exprs, handlers, requires, provides, outputs, playAttrs = [], defaultVars = [], source}
  where
    mkHandlers task = case task.params of
      Handler n v -> (n, task.module_, [(task.module_, v)] <> filter ((/=) "listen" . fst) task.attrs)
      _ -> error "The impossible has happened: non handler can't be in handler list"
    -- Cleanup unrelated resource.
    updateAvailable = filter (not . unusedResource . (.dep))
    unusedResource = \case
      Command fp -> source == fp
      _ -> False

normalizeDefinition :: FilePath -> Text -> [Task] -> State Env Definition
normalizeDefinition = normalizeDefinitionWithHandlers []

normalizePlay :: Play -> State Env Definition
normalizePlay play = do
  Binder name <- freshName "play" (playName play)
  addPlayAttrs <$> normalizeDefinitionWithHandlers play.handlers play.playPath name play.tasks
  where
    addPlayAttrs def = def {playAttrs = play.attrs}

-- | Extract the hosts from a play attributes:
-- >>> playName BasePlay {tasks = [], handlers = [], playPath = "", attrs = [("hosts", [json|"localhost"|])]}
-- "localhost"
playName :: Play -> Text
playName play = fromMaybe "" (preview _String =<< lookup "hosts" play.attrs)

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
        exprs <- traverse topLevelCall (zip plays defs)
        pure $ topLevel exprs : defs
   in solveInputs (xs <> env.definitions)
  where
    topLevelCall (play, def) = do
      binder <- freshName "results" (playName play)
      let term = DefinitionCall {defName = def.name, taskVars = []}
          outputs = Right []
          when_ = Nothing
          taskAttrs = []
          rescue = False
      pure $ Expr {binder, requires = def.requires, provides = def.provides, outputs, inputs = [], loop = Nothing, term, taskAttrs, when_, rescue}
    topLevel :: [Expr] -> Definition
    topLevel exprs = (emptyDefinition "playbook" "") {exprs}
