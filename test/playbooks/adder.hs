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
main = runHaxible "inventory.yaml" "test/playbooks/adder.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
      src = ""
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  resultsLocalhost1 <- playLocalhost1 playAttrs (taskAttrs) ([("answer", resultsLocalhost0 !! 0)] <> taskVars)
  pure $ resultsLocalhost0 <> resultsLocalhost1

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> parentPlayAttrs
      src = "test/playbooks"
  debug0 <- runTask src playAttrs "debug" ([("debug", [json|{"msg":"42"}|])] <> taskAttrs) (taskVars)
  assert0 <- runTask src playAttrs "assert" ([("assert", [json|{"that":["answer['msg'] == '42'"]}|])] <> taskAttrs) ([("answer", debug0)] <> taskVars)
  pure $ [debug0] <> [assert0]

playLocalhost1 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost1 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> parentPlayAttrs
      src = "test/playbooks"
  resultsAdder0 <- roleAdder0 playAttrs (taskAttrs) ([("x", [json|"{{ answer['msg'] }}"|]), ("y", [json|"21"|]), ("adder_version", [json|"42 {{ adder_commit | default('HEAD') }}"|])] <> taskVars)
  debug1 <- runTask src playAttrs "debug" ([("debug", [json|{"msg":"Over!"}|])] <> taskAttrs) (taskVars)
  pure $ resultsAdder0 <> [debug1]

roleAdder0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
roleAdder0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
      src = "test/playbooks/roles/adder"
  debugAddingNumbers0 <- runTask src playAttrs "debug" ([("debug", [json|{"msg":"Adding {{ x }} + {{ y }} with {{ adder_version }}"}|]), ("name", [json|"Adding numbers"|])] <> taskAttrs) (taskVars)
  pure $ [debugAddingNumbers0]

