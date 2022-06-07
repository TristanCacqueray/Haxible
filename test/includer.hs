-- Generated with haxible
{-# LANGUAGE QuasiQuotes, ApplicativeDo, OverloadedStrings #-}
{- cabal:
build-depends: base, haxible
ghc-options: -threaded -rtsopts -with-rtsopts=-N -with-rtsopts=-T
-}
module Main (main) where

import Haxible.Eval

main :: IO ()
main = runHaxible "inventory.yaml" (playbook [] [])

playbook :: Vars -> Vars -> AnsibleHaxl [Value]
playbook playAttrs baseEnv = do
  resultsLocalhost0 <- playLocalhost0 ([("hosts", [json|"localhost"|])] <> playAttrs) ([] <> [] <> baseEnv)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs baseEnv = do
  tasksTaskstasksgreetyaml0 <- traverseInclude (\__haxible_loop_item ->  tasksTasksGreetYaml ([] <> playAttrs) ([("item", __haxible_loop_item)] <> [("include_param", [json|"{{ item }}"|])] <> baseEnv) )  [[json|"Haxible"|], [json|"World"|]]
  debug0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"Result is {{ included_result }},\nnested {{ nested_included_result}}\n"}}|] ([("included_result", tasksTaskstasksgreetyaml0 !! 0), ("nested_included_result", tasksTaskstasksgreetyaml0 !! 2)] <> baseEnv)
  pure $ tasksTaskstasksgreetyaml0 <> [debug0]

tasksTasksGreetYaml :: Vars -> Vars -> AnsibleHaxl [Value]
tasksTasksGreetYaml playAttrs baseEnv = do
  debugIncludedTask0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"Hello {{ include_param }}"},"name":"Included task"}|] ([] <> baseEnv)
  tasksTasksothertasksyaml0 <- tasksOtherTasksYaml ([] <> playAttrs) ([] <> [] <> baseEnv)
  pure $ [debugIncludedTask0] <> tasksTasksothertasksyaml0

tasksOtherTasksYaml :: Vars -> Vars -> AnsibleHaxl [Value]
tasksOtherTasksYaml playAttrs baseEnv = do
  debugUnusedInclude0 <- runTask playAttrs "debug" [json|{"debug":null,"name":"Unused include"}|] ([] <> baseEnv)
  debugNestedIncludedTask0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"Nested {{ include_param }}"},"name":"Nested included task"}|] ([] <> baseEnv)
  pure $ [debugUnusedInclude0] <> [debugNestedIncludedTask0]

