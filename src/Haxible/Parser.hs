module Haxible.Parser (decodePlaybook, renderScript) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (toStrict)
import Data.Foldable
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Text qualified as Text
import Data.Text.Encoding
import Haxible.Play
import System.FilePath

decodePlaybook :: FilePath -> IO Playbook
decodePlaybook fp =
  Playbook <$> (traverse (resolveHostPlay fp) =<< decodeFile fp)

annotateDependency :: [Task] -> [Task]
annotateDependency = reverse . fst . foldl' go ([], [])
  where
    go :: ([Task], [Text]) -> Task -> ([Task], [Text])
    go (xs, available) task = (task {requires = requires} : xs, newAvailable)
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

resolveHostPlay :: FilePath -> HostPlay -> IO HostPlay
resolveHostPlay source hostPlay = do
  tasks <-
    annotateDependency . concat <$> traverse resolveImport hostPlay.tasks
  pure $ hostPlay {tasks}
  where
    resolveImport :: Task -> IO [Task]
    resolveImport task = case task.action of
      "include_role" -> do
        let role_name = unpack $ fromMaybe "missing name" $ preview (key "name" . _String) $ task.attributes
            role_path = takeDirectory source </> "roles" </> role_name </> "tasks" </> "main.yaml"
        concat <$> (traverse resolveImport =<< decodeFile @[Task] role_path)
      _ -> pure [task]

renderScript :: Playbook -> Text
renderScript (Playbook plays) =
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
      <> ((\play -> "  play_" <> play.hosts) <$> plays)
      <> ["  pure ()", ""]
      <> concatMap renderPlay plays
  where
    renderPlay :: HostPlay -> [Text]
    renderPlay play =
      [ "play_" <> play.hosts <> " :: AnsibleHaxl ()",
        "play_" <> play.hosts <> " = do"
      ]
        <> (mappend "  " . renderCode play.hosts <$> play.tasks)
        <> ["  pure ()"]

quote :: Text -> Text
quote = Text.cons '"' . flip Text.snoc '"'

renderCode :: Text -> Task -> Text
renderCode host task = Text.unwords (registerBind <> taskCall)
  where
    registerBind = case task.register of
      Just v -> [v, "<-"]
      Nothing -> []

    taskCall = case task.loop of
      Nothing -> directCall
      Just xs -> ["traverse", "(\\item -> "] <> directCall <> [") " <> Text.pack (show xs)]

    directCall = ["runTask", quote host, "(" <> Text.pack (show task.name) <> ")", quote task.action, arguments]

    arguments = case requirements of
      [] -> attributes
      xs -> "(renderTemplates [" <> Text.intercalate ", " (mkArg <$> xs) <> "] " <> attributes <> ")"

    attributes = "[json| " <> decodeUtf8 (Data.ByteString.toStrict $ Data.Aeson.encode task.attributes) <> " |]"

    requirements = case task.loop of
      Nothing -> task.requires
      Just _ -> "item" : task.requires

    mkArg v = "(" <> quote v <> ", " <> v <> ")"
