module Haxible.Codegen (renderScript) where

import Data.Aeson (encode)
import Data.Text qualified as Text
import Haxible.Normalize
import Haxible.Prelude

renderScript :: FilePath -> FilePath -> [Definition] -> Text
renderScript inventory playPath defs =
  Text.unlines $
    [ "#!/usr/bin/env cabal",
      "-- Generated with haxible",
      "{-# LANGUAGE QuasiQuotes, ApplicativeDo, OverloadedStrings #-}",
      "{- cabal:",
      "build-depends: base, haxible",
      "ghc-options: -threaded -rtsopts -with-rtsopts=-N -with-rtsopts=-T",
      "-}",
      "module Main (main) where\n",
      "import Haxible.Eval\n",
      "main :: IO ()",
      "main = runHaxible " <> quote (from inventory) <> " " <> quote (from playPath) <> " (playbook [] [])\n"
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
      Nothing
        | extractFact -> ["extractFact", "<$>"] <> callExpr
        | otherwise -> callExpr
      where
        traverser = case e.term of
          ModuleCall _ -> "traverseLoop"
          DefinitionCall _ -> "traverseInclude"
          BlockRescueCall _ -> "traverseInclude"
        mkTraverse arg = [traverser, "(\\__haxible_loop_item -> "] <> callExpr <> [") ", arg]

    (extractFact, callExpr) = case e.term of
      ModuleCall CallModule {module_, params, taskAttrs} ->
        ( module_ == "set_fact",
          [ "runTask",
            "playAttrs",
            quote module_,
            embedJSON (mkObj $ [(module_, params)] <> taskAttrs),
            paren (requirements <> " <> baseEnv")
          ]
        )
      DefinitionCall CallDefinition {name, playAttrs, baseEnv} ->
        (False, [name] <> cdExpr playAttrs baseEnv)
      BlockRescueCall CallDefinition {name, playAttrs, baseEnv} ->
        (False, ["tryRescue", name <> "Main", name <> "Rescue"] <> cdExpr playAttrs baseEnv)

    cdExpr playAttrs baseEnv =
      [ paren (textList (mkJsonArg <$> playAttrs) <> " <> playAttrs"),
        paren (requirements <> " <> " <> textList (mkJsonArg <$> baseEnv) <> " <> baseEnv")
      ]

-- | Add parenthesis
-- >>> paren "toto"
-- "(toto)"
paren :: Text -> Text
paren = Text.cons '(' . flip Text.snoc ')'

-- | Create json quasi quote
-- >>> embedJSON Null
-- "[json|null|]"
embedJSON :: Value -> Text
embedJSON v = "[json|" <> unsafeFrom (Data.Aeson.encode v) <> "|]"

-- | Create task arguments
-- >>> mkJsonArg (("test", Null))
-- "(\"test\", [json|null|])"
mkJsonArg :: (Text, Value) -> Text
mkJsonArg (n, v) = "(" <> quote n <> ", " <> embedJSON v <> ")"

-- | Format a text list
-- >>> textList ["a", "b"]
-- "[a, b]"
textList :: [Text] -> Text
textList xs = "[" <> Text.intercalate ", " xs <> "]"
