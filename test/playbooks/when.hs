#!/usr/bin/env cabal
-- Generated with haxible
{-# LANGUAGE QuasiQuotes, ApplicativeDo, OverloadedStrings #-}
{- cabal:
build-depends: base, haxible
ghc-options: -threaded -rtsopts -with-rtsopts=-N -with-rtsopts=-T
-}
module Main (main) where

import Haxible.Eval

main :: IO ()
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/when.yaml" expect (playbook [] [])
  where expect = [[json|{"changed":false,"msg":"Print task Hello!"}|], [json|{"changed":false,"skip_reason":"Conditional result was False"}|], [json|{"changed":false,"skip_reason":"Conditional result was False"}|], [json|{"changed":false,"skip_reason":"Conditional result was False"}|]]

playbook :: Vars -> Vars -> AnsibleHaxl [Value]
playbook playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = ""
  resultsPlayLocalhost0 <- playLocalhost0 playAttrs (localVars <> defaultVars)
  pure $ resultsPlayLocalhost0

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsTasksTasksPrintYaml0 <- tasksTasksPrintYaml0 playAttrs ([("print_arg", [json|"Hello!"|])] <> localVars <> defaultVars)
  let when_ = False
  resultsTasksTasksPrintYaml1 <- if when_ then (tasksTasksPrintYaml1 playAttrs ([("print_arg", [json|"Hello!"|])] <> localVars <> defaultVars)) else pure [[json|{"changed":false,"skip_reason":"Conditional result was False"}|]]
  let when_ = True
  resultsBlock0 <- if when_ then (block0 playAttrs (localVars <> defaultVars)) else pure [[json|{"changed":false,"skip_reason":"Conditional result was False"}|]]
  when_ <- all extractWhen <$> sequence [runTask "" playAttrs defaultVars "debug" [("name", [json|"Resolving template {{ true or false }}"|]), ("debug", [json|{"msg":"{{ true or false }}"}|])] localVars, runTask "" playAttrs defaultVars "debug" [("name", [json|"Resolving template {{ true and false }}"|]), ("debug", [json|{"msg":"{{ true and false }}"}|])] localVars]
  debug3 <- if when_ then (runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Should be skipped"}|])]) localVars) else pure [json|{"changed":false,"skip_reason":"Conditional result was False"}|]
  pure $ resultsTasksTasksPrintYaml0 <> resultsTasksTasksPrintYaml1 <> resultsBlock0 <> [debug3]

block0 :: Vars -> Vars -> AnsibleHaxl [Value]
block0 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks"
  let when_ = False
  debug2 <- if when_ then (runTask src playAttrs defaultVars "debug" ([("debug", [json|null|])]) localVars) else pure [json|{"changed":false,"skip_reason":"Conditional result was False"}|]
  pure $ [debug2]

tasksTasksPrintYaml1 :: Vars -> Vars -> AnsibleHaxl [Value]
tasksTasksPrintYaml1 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks/./tasks"
  debug1 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Print task {{ print_arg }}"}|])]) localVars
  pure $ [debug1]

tasksTasksPrintYaml0 :: Vars -> Vars -> AnsibleHaxl [Value]
tasksTasksPrintYaml0 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks/./tasks"
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Print task {{ print_arg }}"}|])]) localVars
  pure $ [debug0]

