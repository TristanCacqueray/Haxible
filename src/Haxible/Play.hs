-- | This module contains the main data types and json decoder
module Haxible.Play (decodeFile, Variables (..), Playbook (..), HostPlay (..), Task (..)) where

import Data.Aeson
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified
import Data.Bifunctor (first)
import Data.ByteString (readFile)
import Data.Text (Text)
import Data.Yaml qualified (decodeEither')

newtype Playbook = Playbook [HostPlay] deriving (Eq, Show)

newtype Variables = Variables [(Text, Value)] deriving (Eq, Show)

instance FromJSON Variables where
  parseJSON =
    withObject "Vars" $
      pure . Variables . map (first Data.Aeson.Key.toText) . Data.Aeson.KeyMap.toList

data HostPlay = HostPlay
  { hosts :: Text,
    tasks :: [Task],
    vars :: Maybe Variables
  }
  deriving (Eq, Show)

instance FromJSON HostPlay where
  parseJSON = withObject "HostPlay" $ \v ->
    HostPlay
      <$> v .: "hosts"
      <*> v .: "tasks"
      <*> v .:? "vars"

data Task = Task
  { name :: Maybe Text,
    action :: Text,
    attributes :: Value,
    requires :: [Text],
    register :: Maybe Text,
    loop :: Maybe [Text],
    vars :: Maybe Variables
  }
  deriving (Eq, Show)

instance FromJSON Task where
  parseJSON = withObject "Task" $ \v -> do
    (action, attributes) <- case filter (\(n, _) -> taskAttribute n) (Data.Aeson.KeyMap.toList v) of
      [(n, attr)] -> pure (Data.Aeson.Key.toText n, attr)
      [] -> error "Missing task"
      xs -> error $ "Unknown task: " <> show xs
    Task
      <$> v .:? "name"
      <*> pure action
      <*> pure attributes
      <*> pure []
      <*> v .:? "register"
      <*> v .:? "loop"
      <*> v .:? "vars"
    where
      taskAttribute n = n `notElem` ["name", "register", "loop"]

decodeFile :: (Show a, FromJSON a) => FilePath -> IO a
decodeFile fp = do
  bs <- Data.ByteString.readFile fp
  pure $ case Data.Yaml.decodeEither' bs of
    Right v -> v
    x -> error $ "Unexpected YAML: " <> show x
