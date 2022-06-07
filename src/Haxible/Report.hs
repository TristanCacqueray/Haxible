module Haxible.Report (reportTiming) where

import Data.Aeson
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Haxible.Prelude

reportTiming :: NonEmpty Value -> Text
reportTiming rawResults =
  Text.unlines $
    ["@startuml", "scale 1 as 100 pixels"]
      <> (mappend "concise " <$> orderedKeys)
      <> concatMap toTiming (Map.toList modules)
      <> ["@enduml"]
  where
    orderedKeys = fst <$> sortOn (\(_, (s, _, _)) -> s) (Map.toList modules)
    results :: [Value]
    results = reverse $ foldl' go mempty rawResults
      where
        go acc x = case preview (key "results" . _Array) x of
          Just xs -> toList xs <> acc
          Nothing -> x : acc

    scaleTime :: Integer -> Integer
    scaleTime x = round (fromInteger (x - minDate) * step)

    toTiming :: (Text, (Integer, Integer, Text)) -> [Text]
    toTiming (name, (scaleTime -> start, scaleTime -> end, name')) =
      [ "\n" <> "@" <> name,
        Text.unwords [from (show start), "is", quote name'],
        Text.unwords [from (show end), "is", "{hidden}"]
      ]

    step = 9 / (unsafeInto @Double $ maxDate - minDate)
    minDate, maxDate :: Integer
    minDate = minimum $ map (getNumber "__haxible_start") results
    maxDate = maximum $ map (getNumber "__haxible_end") results
    getNumber key' = round . fromMaybe (error $ "missing attributes: " <> from key') . preview (key key' . _Number)
    modules :: Map Text (Integer, Integer, Text)
    modules = foldl' (flip go) mempty results
      where
        go :: Value -> Map Text (Integer, Integer, Text) -> Map Text (Integer, Integer, Text)
        go v acc = Map.insert moduleID (getNumber "__haxible_start" v, getNumber "__haxible_end" v, name) acc
          where
            moduleID = head $ filter (`Map.notMember` acc) $ map (\i -> module_ <> from (show @Integer i)) [0 ..]
            module_ = fromMaybe (error "no module") (preview (key "__haxible_module" . _String) v)
            name = case toList $ fromMaybe (error "no name?") (preview (key "__haxible_play" . key "tasks" . _Array) v) of
              [x] -> fromMaybe "task" (preview (key "name" . _String) x)
              _ -> error "No task found"
