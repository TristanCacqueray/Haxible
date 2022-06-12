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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/includer.yaml" expect (playbook [] [])
  where expect = []

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
  let loop_ = [[json|"Haxible"|], [json|"World"|]]
  let loopFun loop_item = do
        tasksTasksGreetYaml0 playAttrs ([("item", loop_item)] <> [("include_param", [json|"{{ item }}"|])] <> localVars <> defaultVars)
  resultsTasksTasksGreetYaml0 <- traverseInclude loopFun loop_
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Result is {{ included_result }},\nnested {{ nested_included_result}}\n"}|])]) ([("included_result", resultsTasksTasksGreetYaml0 !! 0), ("nested_included_result", resultsTasksTasksGreetYaml0 !! 2)] <> localVars)
  pure $ resultsTasksTasksGreetYaml0 <> [debug0]

tasksTasksGreetYaml0 :: Vars -> Vars -> AnsibleHaxl [Value]
tasksTasksGreetYaml0 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks/./tasks"
  debugIncludedTask0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Hello {{ include_param }}"}|]), ("name", [json|"Included task"|])]) localVars
  resultsTasksOtherTasksYaml0 <- tasksOtherTasksYaml0 playAttrs (localVars <> defaultVars)
  pure $ [debugIncludedTask0] <> resultsTasksOtherTasksYaml0

tasksOtherTasksYaml0 :: Vars -> Vars -> AnsibleHaxl [Value]
tasksOtherTasksYaml0 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks/./tasks/."
  debugUnusedInclude0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|null|]), ("name", [json|"Unused include"|])]) localVars
  debugNestedIncludedTask0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Nested {{ include_param }}"}|]), ("name", [json|"Nested included task"|])]) localVars
  pure $ [debugUnusedInclude0] <> [debugNestedIncludedTask0]

