-- | This module contains JSON decoder for the raw syntax.
module Haxible.Syntax
  ( decodeFile,
    Vars (..),
    BasePlay (..),
    BaseTask (..),
    TaskSyntax,
    PlaySyntax,
  )
where

import Data.Aeson
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified
import Data.ByteString (readFile)
import Data.Yaml qualified (decodeEither')
import Haxible.Prelude

data BasePlay task = BasePlay
  { tasks :: [task],
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

newtype Vars = Vars [(Text, Value)]
  deriving (Eq, Show)

type TaskSyntax = BaseTask Value

type PlaySyntax = BasePlay TaskSyntax

items :: [Text] -> Data.Aeson.KeyMap.KeyMap Value -> [(Text, Value)]
items xs = filter (isUnknown . fst) . map (first Data.Aeson.Key.toText) . Data.Aeson.KeyMap.toList
  where
    isUnknown x = x `notElem` xs

instance FromJSON Vars where
  parseJSON = withObject "Vars" $ pure . Vars . items []

instance FromJSON PlaySyntax where
  parseJSON = withObject "BasePlay" $ \v ->
    BasePlay
      <$> v .: "tasks"
      <*> pure (items ["tasks"] v)

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
      <*> pure (items [module_] v)
    where
      nonModuleAttributes = ["name", "register", "loop", "vars"]

decodeFile :: (Show a, FromJSON a, MonadIO m) => FilePath -> m a
decodeFile fp = do
  bs <- liftIO (Data.ByteString.readFile fp)
  pure $ case Data.Yaml.decodeEither' bs of
    Right v -> v
    x -> error $ fp <> ": Unexpected YAML: " <> show x
