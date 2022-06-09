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
      "main = runHaxible "
        <> Text.unwords
          [ quote (from inventory),
            quote (from playPath),
            "(playbook [] [] [])\n"
          ]
    ]
      <> concatMap renderDefinition defs

renderDefinition :: Definition -> [Text]
renderDefinition def =
  [ def.name <> " :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]",
    def.name <> " parentPlayAttrs taskAttrs taskVars = do",
    "  let playAttrs = " <> concatList [playAttrs, "parentPlayAttrs"],
    "      src = " <> quote (from def.source)
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
renderExpr e = whenBinder <> loopBinder <> [from e.binder <> " <- " <> Text.unwords finalExpr]
  where
    singleResult = case e.term of
      ModuleCall _ | isNothing e.loop -> True
      _ -> False
    extractFact = case e.term of
      ModuleCall cm | cm.module_ == "set_fact" -> True
      _ -> False

    -- Bind the when value if necessary
    whenBinder = case e.when_ of
      Just (Bool True) -> ["let when_ = True"]
      Just (Bool False) -> ["let when_ = False"]
      Just (String v) -> [Text.unwords ["when_", "<-", "extractWhen", "<$>", callDebug (toJinja v)]]
      Just (Array v) -> [Text.unwords ["when_", "<-", "all extractWhen <$> sequence", textList (callDebug <$> (toJinjas <$> toList v))]]
      Just v -> error $ "Expected a string for when, got: " <> unsafeFrom (encode v)
      Nothing -> []

    loopBinder = case e.loop of
      Just (Array xs) -> ["let loop_ = " <> textList (embedJSON <$> toList xs)]
      Just (String v) -> [Text.unwords ["loop_", "<-", "extractLoop", "<$>", callDebug v]]
      Just v -> error $ "Invalid loop expression: " <> unsafeFrom (encode v)
      Nothing -> []

    callDebug = debugCall (filter (not . loopVar) e.requirements)
      where
        loopVar req
          | req.origin == LoopVar = True
          | otherwise = False

    finalExpr
      | isJust e.when_ = ["if when_ then", paren (Text.unwords loopExpr), "else pure", skipResult]
      | otherwise = loopExpr

    skipResult
      | singleResult = embedJSON skippedOutput
      | otherwise = textList (embedJSON <$> skipResults [] e.outputs)
      where
        skipResults acc = \case
          Left (Environment xs) -> concatMap (skipResults acc . snd) xs
          Right _ -> skippedOutput : acc

        skippedOutput = [json|{"changed":false,"skip_reason":"Conditional result was False"}|]

    loopExpr
      | isJust e.loop = mkTraverse "loop_"
      | extractFact = ["extractFact", "<$>"] <> callExpr
      | otherwise = callExpr
      where
        traverser = case e.term of
          ModuleCall _ -> "traverseLoop"
          DefinitionCall _ -> "traverseInclude"
          BlockRescueCall _ -> "traverseInclude"
        mkTraverse arg = [traverser, "(\\__haxible_loop_item -> "] <> callExpr <> [") ", arg]

    vars = paren (concatList [textReq e.requirements, "taskVars"])

    callExpr = case e.term of
      ModuleCall CallModule {module_, params} ->
        [ "runTask src playAttrs",
          quote module_,
          paren (concatList [textList (mkJsonArg <$> [(module_, params)] <> e.taskAttrs), "taskAttrs"]),
          vars
        ]
      DefinitionCall CallDefinition {name, taskVars} ->
        [name, "playAttrs"] <> callParams e.taskAttrs taskVars
      BlockRescueCall CallDefinition {name, taskVars} ->
        ["tryRescue", paren (name <> "Main playAttrs"), paren (name <> "Rescue playAttrs")]
          <> callParams e.taskAttrs taskVars

    callParams taskAttrs taskVars =
      [ paren (concatList [textList (mkJsonArg <$> taskAttrs), "taskAttrs"]),
        paren (concatList [textReq e.requirements, textList (mkJsonArg <$> taskVars), "taskVars"])
      ]

textReq :: [Requirement] -> Text
textReq xs = textList $ (\req -> "(" <> quote req.name <> ", " <> mkOrigin req.origin <> ")") <$> xs
  where
    mkOrigin = \case
      Direct n -> from n
      Nested n i -> from n <> " !! " <> from (show i)
      LoopVar -> "__haxible_loop_item"

-- Call the debug module to evaluate a string template
debugCall :: [Requirement] -> Text -> Text
debugCall reqs template =
  Text.unwords
    [ "runTask \"\" playAttrs",
      quote "debug",
      textList
        ( mkJsonArg
            <$> [ ("name", String ("Resolving template " <> template)),
                  ("debug", mkObj [("msg", String template)])
                ]
        ),
      paren (concatList [textReq reqs, "taskVars"])
    ]

toJinja :: Text -> Text
toJinja v = Text.unwords ["{{", v, "}}"]

toJinjas :: Value -> Text
toJinjas = \case
  String v -> toJinja v
  v -> error $ "Expected a string, got: " <> unsafeFrom (encode v)

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
