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
main = runHaxible "inventory.yaml" "test/playbooks/when.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|])] <> parentPlayAttrs
  resultsTaskstasksprintyaml00 <- tasksTasksPrintYaml0 playAttrs (taskAttrs) ([("print_arg", [json|"Hello!"|])] <> taskVars)
  let when_ = False
  resultsTaskstasksprintyaml10 <- if when_ then (tasksTasksPrintYaml1 playAttrs (taskAttrs) ([("print_arg", [json|"Hello!"|])] <> taskVars)) else pure [[json|{"changed":false,"skip_reason":"Conditional result was False"}|]]
  let when_ = True
  block0 <- if when_ then (block0 playAttrs (taskAttrs) (taskVars)) else pure [[json|{"changed":false,"skip_reason":"Conditional result was False"}|]]
  when_ <- all extractWhen <$> sequence [runTask playAttrs "debug" [("debug", [json|{"msg":"{{ true or false }}"}|])] (taskVars), runTask playAttrs "debug" [("debug", [json|{"msg":"{{ true and false }}"}|])] (taskVars)]
  debug3 <- if when_ then (runTask playAttrs "debug" ([("debug", [json|{"msg":"Should be skipped"}|])] <> taskAttrs) (taskVars)) else pure [json|{"changed":false,"skip_reason":"Conditional result was False"}|]
  pure $ resultsTaskstasksprintyaml00 <> resultsTaskstasksprintyaml10 <> block0 <> [debug3]

block0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
block0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  let when_ = False
  debug2 <- if when_ then (runTask playAttrs "debug" ([("debug", [json|null|])] <> taskAttrs) (taskVars)) else pure [json|{"changed":false,"skip_reason":"Conditional result was False"}|]
  pure $ [debug2]

tasksTasksPrintYaml1 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
tasksTasksPrintYaml1 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  debug1 <- runTask playAttrs "debug" ([("debug", [json|{"msg":"Print task {{ print_arg }}"}|])] <> taskAttrs) (taskVars)
  pure $ [debug1]

tasksTasksPrintYaml0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
tasksTasksPrintYaml0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  debug0 <- runTask playAttrs "debug" ([("debug", [json|{"msg":"Print task {{ print_arg }}"}|])] <> taskAttrs) (taskVars)
  pure $ [debug0]

