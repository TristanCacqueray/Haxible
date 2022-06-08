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
main = runHaxible "inventory.yaml" "test/playbooks/includer.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|])] <> parentPlayAttrs
  resultsTaskstasksgreetyaml00 <- traverseInclude (\__haxible_loop_item ->  tasksTasksGreetYaml0 playAttrs (taskAttrs) ([("item", __haxible_loop_item)] <> [("include_param", [json|"{{ item }}"|])] <> taskVars) )  [[json|"Haxible"|], [json|"World"|]]
  debug0 <- runTask playAttrs "debug" ([("debug", [json|{"msg":"Result is {{ included_result }},\nnested {{ nested_included_result}}\n"}|])] <> taskAttrs) ([("included_result", resultsTaskstasksgreetyaml00 !! 0), ("nested_included_result", resultsTaskstasksgreetyaml00 !! 2)] <> taskVars)
  pure $ resultsTaskstasksgreetyaml00 <> [debug0]

tasksTasksGreetYaml0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
tasksTasksGreetYaml0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  debugIncludedTask0 <- runTask playAttrs "debug" ([("debug", [json|{"msg":"Hello {{ include_param }}"}|])] <> taskAttrs) (taskVars)
  resultsTasksothertasksyaml00 <- tasksOtherTasksYaml0 playAttrs (taskAttrs) (taskVars)
  pure $ [debugIncludedTask0] <> resultsTasksothertasksyaml00

tasksOtherTasksYaml0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
tasksOtherTasksYaml0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  debugUnusedInclude0 <- runTask playAttrs "debug" ([("debug", [json|null|])] <> taskAttrs) (taskVars)
  debugNestedIncludedTask0 <- runTask playAttrs "debug" ([("debug", [json|{"msg":"Nested {{ include_param }}"}|])] <> taskAttrs) (taskVars)
  pure $ [debugUnusedInclude0] <> [debugNestedIncludedTask0]

