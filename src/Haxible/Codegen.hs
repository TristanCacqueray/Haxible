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
renderExpr e = preCode <> [from e.binder <> " <- " <> Text.unwords finalExpr]
  where
    (preCode, finalExpr) = case e.loop of
      Just v ->
        ( mconcat
            [ -- Get the loop_ value
              loopBinder v,
              -- Create a local function to resolve the when_ value per loop item
              ["let loopFun loop_item = do"]
                <> (mappend "      " <$> (whenBinder <> whenExpr callExpr))
            ],
          let traverser = case e.term of
                ModuleCall _ -> "traverseLoop"
                DefinitionCall _ -> "traverseInclude"
                BlockRescueCall _ -> "traverseInclude"
           in [traverser, "loopFun", "loop_"]
        )
      Nothing -> (whenBinder, whenExpr (factExtract <> callExpr))

    whenExpr inner
      | isJust e.when_ = [Text.unwords ["if when_ then", paren (Text.unwords inner), "else pure", skipResult]]
      | otherwise = [Text.unwords inner]
      where
        skipResult = case e.term of
          ModuleCall _ -> embedJSON skippedOutput
          _ -> textList (embedJSON <$> skipResults [] e.outputs)
        skipResults acc = \case
          Left (Environment xs) -> concatMap (skipResults acc . snd) xs
          Right _ -> skippedOutput : acc
        skippedOutput = [json|{"changed":false,"skip_reason":"Conditional result was False"}|]

    -- Bind the when value
    whenBinder = case e.when_ of
      Just (Bool True) -> ["let when_ = True"]
      Just (Bool False) -> ["let when_ = False"]
      Just (String v) -> [Text.unwords ["when_", "<-", "extractWhen", "<$>", callDebug (toJinja v)]]
      Just (Array v) -> [Text.unwords ["when_", "<-", "all extractWhen <$> sequence", textList (callDebug <$> (toJinjas <$> toList v))]]
      Just v -> error $ "Expected a string for when, got: " <> unsafeFrom (encode v)
      Nothing -> []
      where
        -- we resolve the when value with every vars
        callDebug = debugCall e.inputs

    -- Bind the loop value
    loopBinder = \case
      Array xs -> ["let loop_ = " <> textList (embedJSON <$> toList xs)]
      String v -> [Text.unwords ["loop_", "<-", "extractLoop", "<$>", callDebug v]]
      v -> error $ "Invalid loop expression: " <> unsafeFrom (encode v)
      where
        -- we resolve the loop value with every vars, but the loop var
        callDebug = debugCall (filter (not . loopVar) e.inputs)
        loopVar req
          | req.origin == LoopVar = True
          | otherwise = False

    -- Inject extractFact call when needed
    factExtract
      | extractFact = ["extractFact", "<$>"]
      | otherwise = []
      where
        extractFact = case e.term of
          ModuleCall cm | cm.module_ == "set_fact" -> True
          _ -> False

    vars = paren (concatList [textReq e.inputs, "taskVars"])

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
        paren (concatList [textReq e.inputs, textList (mkJsonArg <$> taskVars), "taskVars"])
      ]

textReq :: [Requirement] -> Text
textReq xs = textList $ (\req -> "(" <> quote req.name <> ", " <> mkOrigin req.origin <> ")") <$> xs
  where
    mkOrigin = \case
      Direct n -> from n
      Nested n i -> from n <> " !! " <> from (show i)
      LoopVar -> "loop_item"

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
