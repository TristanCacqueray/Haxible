-- | This module contains the logic to render the Haxl code.
module Haxible.Codegen (renderScript) where

import Data.Aeson (encode)
import Data.Text qualified as Text
import Haxible.Normalize
import Haxible.Prelude

renderScript :: FilePath -> FilePath -> [Definition] -> [Value] -> Text
renderScript inventory playPath defs expectedResults =
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
      "main = Haxible.Eval.runHaxible "
        <> Text.unwords
          [ quote (from inventory),
            quote (from playPath),
            "expect (playbook [] [])"
          ],
      "  where expect = " <> textList (embedJSON <$> expectedResults),
      ""
    ]
      <> concatMap renderDefinition defs

renderDefinition :: Definition -> [Text]
renderDefinition def =
  [ def.name <> " :: Vars -> Vars -> AnsibleHaxl [Value]",
    def.name <> " playAttrs' localVars = do",
    -- The initial playbook does not have playAttrs (because it is a list of play),
    -- Then each individual play adds its playAttrs to the list,
    -- Finally every other definitions will use the playAttrs of the caller.
    "  let playAttrs = " <> concatList [playAttrs, "playAttrs'"],
    "      defaultVars = " <> textList (mkJsonArg <$> def.defaultVars),
    "      src = " <> quote (from def.source)
  ]
    <> (mappend "  " <$> concatMap renderExpr def.exprs)
    <> handlersOrReturn
    <> [""]
  where
    handlersOrReturn = case def.handlers of
      [] -> ["  pure $ " <> outputList]
      xs ->
        -- When there are handlers defined, call their definition with the list of results.
        [ "  let res = " <> outputList,
          "  " <> def.name <> "Handlers playAttrs res",
          "  pure res",
          ""
        ]
          <> renderHandlers def.name xs
    playAttrs = textList (mkJsonArg <$> def.playAttrs)
    outputList = Text.intercalate " <> " (toOutput <$> def.exprs)
    toOutput expr = case expr.term of
      -- Module call produces a single value
      ModuleCall {} -> "[" <> from expr.binder <> "]"
      -- Otherwise definitions provides a list of values
      _ -> from expr.binder

renderHandlers :: Text -> [(Text, Text, Vars)] -> [Text]
renderHandlers name handlers =
  [ name <> "Handlers :: Vars -> [Value] -> AnsibleHaxl ()",
    name <> "Handlers playAttrs res = do"
  ]
    <> (mappend "  " <$> map renderHandler handlers)
  where
    -- Call every handler with the result, the 'notifyHandler' runs the task when one of the result notifies it
    renderHandler (handler, action, taskAttrs) =
      Text.unwords
        ["notifyHandler playAttrs res", quote handler, quote action, textList (mkJsonArg <$> taskAttrs)]

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
                ModuleCall {} -> "traverseLoop"
                DefinitionCall {} -> "traverseInclude"
           in [traverser, "loopFun", "loop_"]
        )
      Nothing -> (whenBinder, whenExpr (factExtract <> callExpr))

    whenExpr (Text.unwords -> inner)
      | isJust e.when_ = [Text.unwords ["if when_ then", paren inner, "else pure", skipResult]]
      | otherwise = [inner]
      where
        -- A fake result list with one value for each expected result.
        skipResult = case e.term of
          ModuleCall {} -> embedJSON skippedOutput
          _ -> textList (embedJSON <$> skipResults [] e.outputs)
        skipResults acc = \case
          Left (Environment xs) -> concatMap (skipResults acc . snd) xs
          Right _ -> skippedOutput : acc
        skippedOutput = [json|{"changed":false,"skip_reason":"Conditional result was False"}|]

    -- Bind the when value
    whenBinder =
      Text.unwords <$> case e.when_ of
        Just (Bool True) -> [["let when_ = True"]]
        Just (Bool False) -> [["let when_ = False"]]
        Just (String v) -> [["when_ <- extractWhen <$>", evalJinja (toJinja v)]]
        -- If the value is a list, then we evaluate each expression and check they are all all True.
        Just (Array v) -> [["when_ <- all extractWhen <$> sequence", textList (evalJinja <$> (toJinjas <$> toList v))]]
        Just v -> error $ "Expected a string for when, got: " <> unsafeFrom (encode v)
        Nothing -> []
      where
        evalJinja = debugCall e.inputs

    -- Bind the loop value
    loopBinder = \case
      Array xs -> ["let loop_ = " <> textList (embedJSON <$> toList xs)]
      String v -> [Text.unwords ["loop_", "<-", "extractLoop", "<$>", evalJinja v]]
      v -> error $ "Invalid loop expression: " <> unsafeFrom (encode v)
      where
        -- we resolve the loop value with every vars, but the loop var
        evalVars = filter (not . loopVar) e.inputs
        evalJinja = debugCall evalVars
        loopVar req
          | req.origin == LoopVar = True
          | otherwise = False

    -- Inject extractFact call when needed
    factExtract
      | extractFact = ["extractFact", "<$>"]
      | otherwise = []
      where
        extractFact = case e.term of
          ModuleCall {module_} | module_ == "set_fact" -> True
          _ -> False

    callExpr = case e.term of
      ModuleCall {module_, params} ->
        [ "runTask src playAttrs defaultVars",
          quote module_,
          paren (textList (mkJsonArg <$> [(module_, params)] <> e.taskAttrs)),
          paren (concatList [textReq e.inputs, "localVars"])
        ]
      DefinitionCall {defName, taskVars}
        | not e.rescue -> [defName, "playAttrs"] <> callParams e.taskAttrs taskVars
        | otherwise ->
            ["tryRescue", paren (defName <> "Main playAttrs"), paren (defName <> "Rescue playAttrs")]
              <> callParams e.taskAttrs taskVars

    callParams taskAttrs taskVars =
      [ paren (concatList [textList (mkJsonArg <$> taskAttrs)]),
        paren (concatList [textReq e.inputs, textList (mkJsonArg <$> taskVars), "localVars"])
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
    [ "runTask \"\" playAttrs defaultVars",
      quote "debug",
      textList
        ( mkJsonArg
            <$> [ ("name", String ("Resolving template " <> template)),
                  ("debug", mkObj [("msg", String template)])
                ]
        ),
      paren (concatList [textReq reqs, "localVars"])
    ]

toJinja :: Text -> Text
toJinja v = Text.unwords ["{{", v, "}}"]

toJinjas :: Value -> Text
toJinjas = \case
  String v -> toJinja v
  v -> error $ "Expected a string, got: " <> unsafeFrom (encode v)

-- | Add parenthesis
-- >>> paren "to to"
-- "(to to)"
paren :: Text -> Text
paren v
  | ' ' `Text.elem` v = Text.cons '(' (Text.snoc v ')')
  | otherwise = v

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
