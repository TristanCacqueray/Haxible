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
  tasksTaskstasksgreetyaml0 <- traverseInclude (\__haxible_loop_item ->  tasksTasksGreetYaml playAttrs (taskAttrs) ([("item", __haxible_loop_item)] <> [("include_param", [json|"{{ item }}"|])] <> taskVars) )  [[json|"Haxible"|], [json|"World"|]]
  debug0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"Result is {{ included_result }},\nnested {{ nested_included_result}}\n"}}|] taskAttrs ([("included_result", tasksTaskstasksgreetyaml0 !! 0), ("nested_included_result", tasksTaskstasksgreetyaml0 !! 2)] <> taskVars)
  pure $ tasksTaskstasksgreetyaml0 <> [debug0]

tasksTasksGreetYaml :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
tasksTasksGreetYaml parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  debugIncludedTask0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"Hello {{ include_param }}"}}|] taskAttrs (taskVars)
  tasksTasksothertasksyaml0 <- tasksOtherTasksYaml playAttrs (taskAttrs) (taskVars)
  pure $ [debugIncludedTask0] <> tasksTasksothertasksyaml0

tasksOtherTasksYaml :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
tasksOtherTasksYaml parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  debugUnusedInclude0 <- runTask playAttrs "debug" [json|{"debug":null}|] taskAttrs (taskVars)
  debugNestedIncludedTask0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"Nested {{ include_param }}"}}|] taskAttrs (taskVars)
  pure $ [debugUnusedInclude0] <> [debugNestedIncludedTask0]

