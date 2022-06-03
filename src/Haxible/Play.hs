-- | This module contains the main data types and json decoder
module Haxible.Play (decodeFile, Variables (..), Playbook (..), HostPlay (..), Task (..)) where

import Data.Aeson
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified
import Data.Bifunctor (first)
import Data.ByteString (readFile)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Yaml qualified (decodeEither')

newtype Playbook = Playbook [HostPlay] deriving (Eq, Show)

newtype Variables = Variables {getVars :: [(Text, Value)]} deriving (Eq, Show)

instance FromJSON Variables where
  parseJSON =
    withObject "Vars" $
      pure . Variables . map (first Data.Aeson.Key.toText) . Data.Aeson.KeyMap.toList

data HostPlay = HostPlay
  { hosts :: Text,
    tasks :: [Task],
    hostVars :: [(Text, Value)],
    playAttrs :: [(Text, Value)]
  }
  deriving (Eq, Show)

items :: Data.Aeson.KeyMap.KeyMap Value -> [(Text, Value)]
items = map (first Data.Aeson.Key.toText) . Data.Aeson.KeyMap.toList

instance FromJSON HostPlay where
  parseJSON = withObject "HostPlay" $ \v ->
    HostPlay
      <$> v .: "hosts"
      <*> v .: "tasks"
      <*> (maybe [] getVars <$> (v .:? "vars"))
      <*> pure (filter (\(n, _) -> unknownPlayAttributes n) (items v))
    where
      unknownPlayAttributes n = n /= "tasks"

data Task = Task
  { name :: Maybe Text,
    taskModule :: (Text, Value),
    requires :: [Text],
    register :: Maybe Text,
    loop :: Value,
    vars :: [(Text, Value)],
    taskAttrs :: [(Text, Value)]
  }
  deriving (Eq, Show)

instance FromJSON Task where
  parseJSON = withObject "Task" $ \v -> do
    (taskModule, attributes) <- case filter (\(n, _) -> taskAttribute n) (items v) of
      [(n, attr)] -> pure (n, attr)
      [] -> error "Missing task"
      xs -> error $ "Unknown task: " <> show xs
    Task
      <$> v .:? "name"
      <*> pure (taskModule, attributes)
      <*> pure []
      <*> v .:? "register"
      <*> (fromMaybe Null <$> v .:? "loop")
      <*> (maybe [] getVars <$> (v .:? "vars"))
      <*> pure (filter (\(n, _) -> rest taskModule n) (items v))
    where
      taskAttribute n = n `notElem` ["name", "register", "loop", "vars"]
      rest taskModule n = n `notElem` [taskModule, "register", "loop"]

decodeFile :: (Show a, FromJSON a) => FilePath -> IO a
decodeFile fp = do
  bs <- Data.ByteString.readFile fp
  pure $ case Data.Yaml.decodeEither' bs of
    Right v -> v
    x -> error $ "Unexpected YAML: " <> show x
