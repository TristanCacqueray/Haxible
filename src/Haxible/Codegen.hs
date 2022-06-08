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
      "main = runHaxible " <> quote (from inventory) <> " " <> quote (from playPath) <> " (playbook [] [] [])\n"
    ]
      <> concatMap renderDefinition defs

renderDefinition :: Definition -> [Text]
renderDefinition def =
  [ def.name <> " :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]",
    def.name <> " parentPlayAttrs taskAttrs taskVars = do",
    "  let playAttrs = " <> concatList [playAttrs, "parentPlayAttrs"]
  ]
    <> (mappend "  " <$> concatMap renderExpr def.exprs)
    <> ["  pure $ " <> outputList, ""]
  where
    playAttrs = textList (mkJsonArg <$> def.playAttrs)
    outputList = Text.intercalate " <> " (toOutput <$> def.exprs)
    toOutput expr = case expr.term of
      -- Module call produces a single value
      ModuleCall _ -> "[" <> from expr.binder <> "]"
      -- Otherwise we got a list of values
      _ -> from expr.binder

renderExpr :: Expr -> [Text]
renderExpr e = whenExpr <> loopExpr <> [from e.binder <> " <- " <> Text.unwords finalWhenExpr]
  where
    toJinja v = Text.unwords ["{{", v, "}}"]
    toJinjas = \case
      String v -> toJinja v
      v -> error $ "Expected a string, got: " <> unsafeFrom (encode v)
    whenExpr = case e.when_ of
      Just (Bool True) -> ["let when_ = True"]
      Just (Bool False) -> ["let when_ = False"]
      Just (String v) -> [Text.unwords ["when_", "<-", "extractWhen", "<$>", debugCall (toJinja v)]]
      Just (Array v) -> [Text.unwords ["when_", "<-", "all extractWhen <$> sequence", textList (debugCall <$> (toJinjas <$> toList v))]]
      Just v -> error $ "Expected a string for when, got: " <> unsafeFrom (encode v)
      Nothing -> []
    debugCall v =
      Text.unwords
        [ "runTask playAttrs",
          quote "debug",
          textList (mkJsonArg <$> [("debug", mkObj [("msg", String v)])]),
          paren (concatList [requirementsWithoutLoop, "taskVars"])
        ]
    finalWhenExpr
      | isJust e.when_ = ["if when_ then", paren (Text.unwords finalExpr), "else pure", skipResult]
      | otherwise = finalExpr
    skipResult
      | singleResult = embedJSON skippedOutput
      | otherwise = textList (embedJSON <$> skipResults [] e.outputs)
    skipResults acc = \case
      Left (Environment xs) -> concatMap (skipResults acc . snd) xs
      Right _ -> skippedOutput : acc

    loopExpr = case e.loop of
      Just (Array xs) -> ["let loop_ = " <> textList (embedJSON <$> toList xs)]
      Just (String v) -> [Text.unwords ["loop_", "<-", "extractLoop", "<$>", debugCall v]]
      Just v -> error $ "Invalid loop expression: " <> unsafeFrom (encode v)
      Nothing -> []

    singleResult = case e.term of
      ModuleCall _ | isNothing e.loop -> True
      _ -> False

    skippedOutput = [json|{"changed":false,"skip_reason":"Conditional result was False"}|]

    requirements = mkReq e.requirements
    mkReq x = textList $ (\req -> "(" <> quote req.name <> ", " <> mkOrigin req.origin <> ")") <$> x
    mkOrigin = \case
      Direct n -> from n
      Nested n i -> from n <> " !! " <> from (show i)
      LoopVar -> "__haxible_loop_item"
    requirementsWithoutLoop = mkReq (filter (not . loopVar) e.requirements)
      where
        loopVar req = case req.origin of
          LoopVar -> True
          _ -> False

    finalExpr
      | isJust e.loop = mkTraverse "loop_"
      | extractFact = ["extractFact", "<$>"] <> callExpr
      | otherwise = callExpr
      where
        traverser = case e.term of
          ModuleCall _ -> "traverseLoop"
          DefinitionCall _ -> "traverseInclude"
          BlockRescueCall _ -> "traverseInclude"
        mkTraverse arg = [traverser, "(\\__haxible_loop_item -> "] <> callExpr <> [") ", arg]

    vars = paren (concatList [requirements, "taskVars"])

    (extractFact, callExpr) = case e.term of
      ModuleCall CallModule {module_, params, taskAttrs} ->
        ( module_ == "set_fact",
          [ "runTask playAttrs",
            quote module_,
            paren (concatList [textList (mkJsonArg <$> [(module_, params)] <> taskAttrs), "taskAttrs"]),
            vars
          ]
        )
      DefinitionCall CallDefinition {name, taskAttrs, taskVars} ->
        (False, [name, "playAttrs"] <> cdExpr taskAttrs taskVars)
      BlockRescueCall CallDefinition {name, taskAttrs, taskVars} ->
        ( False,
          [ "tryRescue",
            paren (name <> "Main playAttrs"),
            paren (name <> "Rescue playAttrs")
          ]
            <> cdExpr taskAttrs taskVars
        )

    cdExpr taskAttrs taskVars =
      [ paren (concatList [textList (mkJsonArg <$> taskAttrs), "taskAttrs"]),
        paren (concatList [requirements, textList (mkJsonArg <$> taskVars), "taskVars"])
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

concatList :: [Text] -> Text
concatList = Text.intercalate " <> " . filter (/= "[]")
