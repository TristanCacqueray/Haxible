-- | This module contains JSON decoder for the raw syntax.
module Haxible.Syntax
  ( decodeFile,
    JsonVars (..),
    BasePlay (..),
    BaseTask (..),
    TaskSyntax,
    PlaySyntax,
    propagableAttrs,
  )
where

import Data.Aeson
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified
import Data.ByteString (readFile)
import Data.Text qualified as Text
import Data.Yaml qualified (decodeEither')
import Haxible.Prelude

data BasePlay task = BasePlay
  { tasks :: [task],
    handlers :: [task],
    playPath :: FilePath,
    -- | The list of attributes such as `become` or `gather_facts`.
    attrs :: [(Text, Value)]
  }
  deriving (Eq, Show)

data BaseTask value = BaseTask
  { name :: Maybe Text,
    module_ :: Text,
    params :: value,
    -- | The list of attributes such as `register` or `loop`
    attrs :: [(Text, Value)]
  }
  deriving (Eq, Show)

newtype Playbook = Playbook [PlaySyntax]
  deriving (Generic, Eq, Show)
  deriving newtype (FromJSON)

newtype JsonVars = JsonVars [(Text, Value)]
  deriving (Eq, Show)

type TaskSyntax = BaseTask Value

type PlaySyntax = BasePlay TaskSyntax

items :: [Text] -> Data.Aeson.KeyMap.KeyMap Value -> [(Text, Value)]
items xs = filter (isUnknown . fst) . map (first Data.Aeson.Key.toText) . Data.Aeson.KeyMap.toList
  where
    isUnknown x = x `notElem` xs

instance FromJSON JsonVars where
  parseJSON = withObject "Vars" $ pure . JsonVars . items []

instance FromJSON PlaySyntax where
  parseJSON = withObject "BasePlay" $ \v -> do
    pre_tasks <- v `getList` "pre_tasks"
    tasks <- v `getList` "tasks"
    roles <- map mkRoleTask <$> (v `getList` "roles")
    handlers <- v `getList` "handlers"
    post_tasks <- v `getList` "post_tasks"
    pure $
      BasePlay (pre_tasks <> roles <> tasks <> post_tasks) handlers "" (items nonPlayAttributes v)
    where
      getList v k = fromMaybe [] <$> v .:? k
      mkRoleTask name =
        BaseTask
          { name = Nothing,
            module_ = "include_role",
            params = mkObj [("name", String name)],
            attrs = []
          }
      nonPlayAttributes = ["pre_tasks", "tasks", "post_tasks", "roles", "handlers"]

instance FromJSON TaskSyntax where
  parseJSON = withObject "BaseTask" $ \v -> do
    (module_, params) <- case items nonModuleAttributes v of
      [(n, attr)] -> pure (n, attr)
      [] -> fail "Missing module"
      xs -> fail $ "Can't pick module from: " <> show (map fst xs)
    BaseTask
      <$> v .:? "name"
      <*> pure module_
      <*> pure params
      <*> pure (first (Text.replace "with_items" "loop") <$> items [module_] v)
    where
      nonModuleAttributes = ["name", "register", "with_items", "loop", "loop_control", "rescue", "vars", "when", "notify", "listen"] <> propagableAttrs

-- | These attributes are passed to the ansible executor, they are not managed by Haxible.
propagableAttrs :: [Text]
propagableAttrs =
  [ "retries",
    "delay",
    "until",
    "changed_when",
    "failed_when",
    "ignore_errors",
    "run_once",
    "delegate_to",
    "no_log",
    "notify",
    "with_first_found"
  ]

decodeFile :: (Show a, FromJSON a, MonadIO m) => FilePath -> m a
decodeFile fp = do
  bs <- liftIO (Data.ByteString.readFile fp)
  pure $ case Data.Yaml.decodeEither' bs of
    Right v -> v
    x -> error $ fp <> ": Unexpected YAML: " <> show x
