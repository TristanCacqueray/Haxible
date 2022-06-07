-- | This module is the core of Haxible.
module Haxible.Normalize
  ( normalizePlaybook,
    Definition (..),
    Expr (..),
    Term (..),
    CallModule (..),
    CallDefinition (..),
    Origin (..),
    Requirement (..),
  )
where

import Data.Char qualified
import Data.Map qualified as Map
import Data.Text qualified as Text
import Haxible.Import
import Haxible.Prelude

-- | A definition is like a function, to represent a play, a role, or a list of tasks.
data Definition = Definition
  { name :: Text,
    requires :: [Resource],
    provides :: [Resource],
    outputs :: Environment,
    exprs :: [Expr]
  }
  deriving (Show, Eq)

-- | An expression is a single instruction.
data Expr = Expr
  { binder :: Binder,
    requires :: [Resource],
    provides :: [Resource],
    outputs :: Either Environment [Resource],
    requirements :: [Requirement],
    loop :: Maybe Value,
    term :: Term
  }
  deriving (Show, Eq)

-- | A term is the expression value.
data Term
  = ModuleCall CallModule
  | DefinitionCall CallDefinition
  | BlockRescueCall CallDefinition
  deriving (Show, Eq)

data CallModule = CallModule {module_ :: Text, params :: Value, taskAttrs :: Vars}
  deriving (Show, Eq)

data CallDefinition = CallDefinition {name :: Text, playAttrs :: Vars, baseEnv :: Vars}
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
    Text.replace "/" "_" p
dependencyValue = \case
  Register n -> n
  Path p -> p

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

-- | propagate binders to sub expression to set the requirements
solveRequirements :: [Definition] -> [Definition]
solveRequirements defs = map updateCallEnv defs
  where
    updateCallEnv :: Definition -> Definition
    updateCallEnv def = def {exprs = reverse . fst . foldl' setCallEnv ([], ([], [])) $ def.exprs}

    outputs = Map.fromList ((\def -> (def.name, def.outputs)) <$> defs)
    getOutputs name = fromMaybe (error $ from name <> ": Unknown def?!") (Map.lookup name outputs)

    setCallEnv :: ([Expr], ReqAcc) -> Expr -> ([Expr], ReqAcc)
    setCallEnv (acc, (avail, nested)) expr = (newExpr : acc, (newAvail, newNested))
      where
        (newAvail, newNested) = case expr.term of
          ModuleCall _ -> ((expr.binder, expr.provides) : avail, nested)
          DefinitionCall dc -> (avail, (expr.binder, getOutputs dc.name) : nested)
          BlockRescueCall rc -> (avail, (expr.binder, getOutputs (rc.name <> "Rescue")) : nested)
        newExpr = expr {requirements = expr.requirements <> directRequirement <> nestedRequirement}

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
-- >>> runState emptyEnv (freshName "role" "my-role")
-- "roleMyRole0"
freshName :: Text -> Text -> State Env Binder
freshName base identifier = do
  names <- gets names
  let name = base <> cleanName identifier
      newName = mappend name $ head $ filter isFresh $ map (from . show @Int) [0 ..]
      isFresh x = name <> x `notElem` names
  modify (\env -> env {names = newName : names})
  pure $ Binder newName

cleanName :: Text -> Text
cleanName = mconcat . map Text.toTitle . Text.split (not . Data.Char.isAlphaNum)

getRequirements :: [Resource] -> [Value] -> [Resource]
getRequirements availables = concatMap findRequirements
  where
    findRequirements :: Value -> [Resource]
    findRequirements v = case v of
      String x -> case filter (\n -> dependencyValue (n.dep) `Text.isInfixOf` x) availables of
        [] -> []
        requirement -> requirement
      Object x -> concatMap findRequirements x
      Array x -> concatMap findRequirements x
      _ -> []

moduleExpr :: Task -> Value -> State Env Expr
moduleExpr task value = do
  binder <- freshName task.module_ (fromMaybe "" task.name)

  availables <- gets availables

  -- Look for requirements and provides
  let requires = getRequirements availables [loop, value, vars]
      provides = Resource binder <$> maybeToList register <> maybeToList destPath
  modify (\env -> env {availables = provides <> availables})

  -- Create the expr
  let term = ModuleCall CallModule {module_ = task.module_, params = value, taskAttrs}
      requirements = []
      outputs = Right provides
  pure $ Expr {binder, requires, provides, outputs, requirements, loop = Nothing, term}
  where
    destPath = Path <$> (getAttr "path" <|> getAttr "dest")
    register = Register <$> (preview _String =<< lookup "register" task.attrs)
    getAttr n = preview (key n . _String) value

    notElemFst = notElem . fst
    taskAttrs = filter (`notElemFst` ["register", "loop", "loop_control"]) task.attrs
    loop = fromMaybe Null $ lookup "loop" task.attrs
    vars = fromMaybe Null $ lookup "vars" task.attrs

roleExpr :: Task -> RoleValue -> State Env Expr
roleExpr task role = do
  when (isJust $ lookup "register" task.attrs) (error "Register include_role is not supported")
  roleDef <- normalizeDefinition name role.tasks
  modify (#definitions %~ (roleDef :))

  expr <- moduleExpr task Null
  binder <- freshName "role" role.name
  pure $ expr {binder, term = DefinitionCall CallDefinition {name, baseEnv, playAttrs = []}}
  where
    baseEnv = taskVars task <> role.defaults
    name = "role" <> cleanName role.name

tasksExpr :: Task -> Text -> [Task] -> State Env Expr
tasksExpr task includeName tasks = do
  when (isJust $ lookup "register" task.attrs) (error "Register include_tasks is not supported")
  tasksDef <- normalizeDefinition name tasks
  modify (#definitions %~ (tasksDef :))

  expr <- moduleExpr task Null
  binder <- freshName "tasks" name
  let outputs = Left tasksDef.outputs
  pure $ expr {binder, outputs, term = DefinitionCall CallDefinition {name, baseEnv = taskVars task, playAttrs = []}}
  where
    name = "tasks" <> cleanName includeName

factsExpr :: Task -> Maybe Value -> Text -> Value -> State Env Expr
factsExpr task cacheable name value = do
  -- exprs
  binder <- freshName "facts" (fromMaybe "" task.name)
  availables <- gets availables
  let requires = getRequirements availables [value]
      resource = Resource {name = binder, dep = Register name}
      provides = [resource]
      outputs = Right provides
      params = mkObj $ [(name, value)] <> maybe [] (\v -> [("cacheable", v)]) cacheable
      term = ModuleCall CallModule {module_ = "set_fact", params, taskAttrs = []}
      loop = Nothing
      requirements = []
  modify (\env -> env {availables = resource : availables})
  -- expr <- moduleExpr task value
  when (isJust (lookup "loop" task.attrs)) $ error "set_fact loop is not supported"
  pure $ Expr {binder, requires, provides, outputs, requirements, loop, term}

blockExpr :: Task -> BlockValue -> State Env Expr
blockExpr task block = do
  binder <- freshName "block" (fromMaybe "" task.name)
  let name = from binder

  (outs, dc) <- case block.rescues of
    [] -> do
      blockDef <- normalizeDefinition name block.tasks
      modify (#definitions %~ (blockDef :))
      pure (blockDef.outputs, DefinitionCall)
    _ -> do
      blockDef <- normalizeDefinition (name <> "Main") block.tasks
      rescueDef <- normalizeDefinition (name <> "Rescue") block.rescues
      modify (#definitions %~ ([rescueDef, blockDef] <>))
      pure (rescueDef.outputs, BlockRescueCall)

  expr <- moduleExpr task Null
  let outputs = Left outs
  pure $ expr {binder, outputs, term = dc CallDefinition {name, baseEnv = taskVars task, playAttrs = []}}

normalizeTask :: Task -> State Env [Expr]
normalizeTask task = do
  exprs <- case task.params of
    Module v -> (: []) <$> moduleExpr task v
    Role r -> (: []) <$> roleExpr task r
    Tasks name xs -> (: []) <$> tasksExpr task name xs
    Facts vars -> traverse (uncurry (factsExpr task Nothing)) vars
    CacheableFacts cacheable vars -> traverse (uncurry (factsExpr task (Just cacheable))) vars
    Block bv -> (: []) <$> blockExpr task bv
  pure $ map addLoopReq exprs
  where
    addLoopReq expr = expr {loop, requirements = extraReq <> expr.requirements}
    (loop, extraReq) = case lookup "loop" task.attrs of
      Just v -> (Just v, [Requirement "item" LoopVar])
      Nothing -> (Nothing, [])

normalizeDefinition :: Text -> [Task] -> State Env Definition
normalizeDefinition name tasks = do
  exprs <- concat <$> traverse normalizeTask tasks
  let provides = nub $ concatMap (.provides) exprs
      requires = nub $ filter (`notElem` provides) $ concatMap (.requires) exprs
      outputs = Environment $ (\e -> (e.binder, e.outputs)) <$> exprs
  pure $ Definition {name, exprs, requires, provides, outputs}

normalizePlay :: Play -> State Env Definition
normalizePlay play = do
  Binder name <- freshName "play" (playName play)
  normalizeDefinition name play.tasks

playName :: Play -> Text
playName play = fromMaybe "" (preview _String =<< lookup "hosts" play.attrs)

taskVars :: Task -> Vars
taskVars task = itoListOf members (fromMaybe Null $ lookup "vars" task.attrs)

-- | Transform a list of 'Play' into a list of 'Definition'.
normalizePlaybook :: [Play] -> [Definition]
normalizePlaybook plays =
  let (xs, env) = flip runState emptyEnv do
        defs <- traverse normalizePlay plays
        exprs <- traverse topLevelCall (zip plays defs)
        pure $ topLevel exprs : defs
   in solveRequirements (xs <> env.definitions)
  where
    topLevelCall (play, def) = do
      binder <- freshName "results" (playName play)
      let term = DefinitionCall CallDefinition {name = def.name, playAttrs = play.attrs, baseEnv = []}
          outputs = Right []
      pure $ Expr {binder, requires = def.requires, provides = def.provides, outputs, requirements = [], loop = Nothing, term}
    topLevel :: [Expr] -> Definition
    topLevel xs =
      Definition
        { name = "playbook",
          requires = [],
          provides = [],
          outputs = Environment [],
          exprs = xs
        }
