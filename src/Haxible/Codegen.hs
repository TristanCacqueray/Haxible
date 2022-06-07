module Haxible.Codegen (renderScript) where

import Data.Aeson (encode)
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified
import Data.Text qualified as Text
import Haxible.Normalize
import Haxible.Prelude

renderScript :: FilePath -> [Definition] -> Text
renderScript inventory defs =
  Text.unlines $
    [ "-- Generated with haxible",
      "{-# LANGUAGE QuasiQuotes, ApplicativeDo, OverloadedStrings #-}",
      "{- cabal:",
      "build-depends: base, haxible",
      "ghc-options: -threaded -rtsopts -with-rtsopts=-N -with-rtsopts=-T",
      "-}",
      "module Main (main) where\n",
      "import Haxible.Eval\n",
      "main :: IO ()",
      "main = runHaxible " <> quote (from inventory) <> " (playbook [] [])\n"
    ]
      <> concatMap renderDefinition defs

renderDefinition :: Definition -> [Text]
renderDefinition def =
  [ def.name <> " :: Vars -> Vars -> AnsibleHaxl [Value]",
    def.name <> " playAttrs baseEnv = do"
  ]
    <> (mappend "  " <$> map renderExpr def.exprs)
    <> ["  pure $ " <> outputList, ""]
  where
    outputList = Text.intercalate " <> " (toOutput <$> def.exprs)
    toOutput expr = case expr.term of
      -- Module call produces a single value
      ModuleCall _ -> "[" <> from expr.binder <> "]"
      -- Otherwise we got a list of values
      _ -> from expr.binder

renderExpr :: Expr -> Text
renderExpr e = from e.binder <> " <- " <> Text.unwords finalExpr
  where
    requirements = textList $ (\req -> "(" <> quote req.name <> ", " <> mkOrigin req.origin <> ")") <$> e.requirements
    mkOrigin = \case
      Direct n -> from n
      Nested n i -> from n <> " !! " <> from (show i)
      LoopVar -> "__haxible_loop_item"

    finalExpr = case e.loop of
      Just (Array xs) -> mkTraverse $ textList (embedJSON <$> toList xs)
      Just (String v) -> mkTraverse $ "(envLoop " <> quote v <> " " <> paren (requirements <> " <> baseEnv") <> ")"
      Just _ -> error $ "Invalid loop expression: " <> show e.loop
      Nothing -> callExpr
      where
        traverser = case e.term of
          ModuleCall _ -> "traverseLoop"
          DefinitionCall _ -> "traverseInclude"
        mkTraverse arg = [traverser, "(\\__haxible_loop_item -> "] <> callExpr <> [") ", arg]

    callExpr = case e.term of
      ModuleCall CallModule {module_, params, taskAttrs} ->
        [ "runTask",
          "playAttrs",
          quote module_,
          embedJSON (obj $ [(module_, params)] <> taskAttrs),
          paren (requirements <> " <> baseEnv")
        ]
      DefinitionCall CallDefinition {name, playAttrs, baseEnv} ->
        [ name,
          paren (textList (mkJsonArg <$> playAttrs) <> " <> playAttrs"),
          paren (requirements <> " <> " <> textList (mkJsonArg <$> baseEnv) <> " <> baseEnv")
        ]

paren :: Text -> Text
paren = Text.cons '(' . flip Text.snoc ')'

quote :: Text -> Text
quote = Text.cons '"' . flip Text.snoc '"'

embedJSON :: Value -> Text
embedJSON v = "[json|" <> unsafeFrom (Data.Aeson.encode v) <> "|]"

mkJsonArg :: (Text, Value) -> Text
mkJsonArg (n, v) = "(" <> quote n <> ", " <> embedJSON v <> ")"

textList :: [Text] -> Text
textList xs = "[" <> Text.intercalate ", " xs <> "]"

obj :: [(Text, Value)] -> Value
obj = Object . Data.Aeson.KeyMap.fromList . map (first Data.Aeson.Key.fromText)
