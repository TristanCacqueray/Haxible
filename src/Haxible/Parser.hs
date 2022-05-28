module Haxible.Parser (decodePlaybook, renderScript) where

import Data.Aeson
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified as HM (lookup, toList)
import Data.ByteString (ByteString, toStrict)
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding
import Data.Vector (toList)
import Data.Yaml

newtype Playbook = Playbook [Task] deriving (Eq, Show)

data Task = Task
  { name :: Maybe Text,
    action :: Text,
    attributes :: Value,
    requires :: [Text],
    register :: Maybe Text,
    loop :: Maybe [Text]
  }
  deriving (Eq, Show)

decodePlaybook :: ByteString -> Playbook
decodePlaybook bs = case Data.Yaml.decodeEither' bs of
  Right (Array v) -> Playbook (reverse $ fst $ foldl' addRequirement ([], []) $ decodeTask <$> Data.Vector.toList v)
  x -> error $ "Invalid pb: " <> show x

addRequirement :: ([Task], [Text]) -> Task -> ([Task], [Text])
addRequirement (xs, available) task = (task {requires = requires} : xs, newAvailable)
  where
    requires = findRequirements task.attributes
    findRequirements :: Value -> [Text]
    findRequirements v = case v of
      String x -> case filter (`Text.isInfixOf` x) available of
        [] -> []
        requirement -> requirement
      Object x -> concatMap findRequirements x
      Array x -> concatMap findRequirements x
      _ -> []
    newAvailable = case task.register of
      Just reg -> reg : available
      Nothing -> available

decodeTask :: Value -> Task
decodeTask (Object obj) =
  let register = decodeTextValue "register"
      name = decodeTextValue "name"
      loop = case HM.lookup "loop" obj of
        Just (Array x) -> traverse decodeText (Data.Vector.toList x)
        _ -> Nothing
      (action, attributes) = case filter (\(n, _) -> taskAttribute n) (HM.toList obj) of
        [(n, v)] -> (Data.Aeson.Key.toText n, v)
        [] -> error "Missing task"
        xs -> error $ "Unknown task: " <> show xs
      requires = []
   in Task {name, action, attributes, requires, register, loop}
  where
    taskAttribute n = Data.Aeson.Key.toText n `notElem` ["name", "register", "loop"]
    decodeTextValue k = decodeText =<< HM.lookup k obj
    decodeText v = case v of
      String x -> Just x
      _ -> Nothing
decodeTask v = error $ "Unexpected task shape: " <> show v

renderScript :: Playbook -> Text
renderScript (Playbook tasks) =
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
      "playbook :: AnsibleHaxl ()",
      "playbook = do"
    ]
      <> (mappend "  " . renderCode <$> tasks)
      <> ["  pure ()"]

quote :: Text -> Text
quote = Text.cons '"' . flip Text.snoc '"'

renderCode :: Task -> Text
renderCode task = Text.unwords (registerBind <> taskCall)
  where
    registerBind = case task.register of
      Just v -> [v, "<-"]
      Nothing -> []

    taskCall = case task.loop of
      Nothing -> directCall
      Just xs -> ["traverse", "(\\item -> "] <> directCall <> [") " <> Text.pack (show xs)]

    directCall = ["runTask", "(" <> Text.pack (show task.name) <> ")", quote task.action, arguments]

    arguments = case requirements of
      [] -> attributes
      xs -> "(renderTemplates [" <> Text.intercalate ", " (mkArg <$> xs) <> "] " <> attributes <> ")"

    attributes = "[json| " <> decodeUtf8 (Data.ByteString.toStrict $ Data.Aeson.encode task.attributes) <> " |]"

    requirements = case task.loop of
      Nothing -> task.requires
      Just _ -> "item" : task.requires

    mkArg v = "(" <> quote v <> ", " <> v <> ")"
