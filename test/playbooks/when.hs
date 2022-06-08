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
  resultsTaskstasksprintyaml10 <- tasksTasksPrintYaml1 playAttrs ([("when", [json|false|])] <> taskAttrs) ([("print_arg", [json|"Hello!"|])] <> taskVars)
  block0 <- block0 playAttrs ([("when", [json|true|])] <> taskAttrs) (taskVars)
  pure $ resultsTaskstasksprintyaml00 <> resultsTaskstasksprintyaml10 <> block0

block0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
block0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  debug2 <- runTask playAttrs "debug" ([("debug", [json|null|]), ("when", [json|false|])] <> taskAttrs) (taskVars)
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

