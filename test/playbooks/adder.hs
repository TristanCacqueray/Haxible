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
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  resultsBackend0 <- playBackend0 playAttrs (taskAttrs) ([("answer", resultsLocalhost0 !! 0)] <> taskVars)
  pure $ resultsLocalhost0 <> resultsBackend0

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> parentPlayAttrs
  debug0 <- runTask playAttrs "debug" ([("debug", [json|{"msg":"42"}|])] <> taskAttrs) (taskVars)
  assert0 <- runTask playAttrs "assert" ([("assert", [json|{"that":["answer['msg'] == '42'"]}|])] <> taskAttrs) ([("answer", debug0)] <> taskVars)
  pure $ [debug0] <> [assert0]

playBackend0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playBackend0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"backend"|])] <> parentPlayAttrs
  resultsAdder0 <- roleAdder0 playAttrs (taskAttrs) ([("x", [json|"{{ answer['msg'] }}"|]), ("y", [json|"21"|])] <> taskVars)
  debug1 <- runTask playAttrs "debug" ([("debug", [json|{"msg":"Over!"}|])] <> taskAttrs) (taskVars)
  pure $ resultsAdder0 <> [debug1]

roleAdder0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
roleAdder0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  debugAddingNumbers0 <- runTask playAttrs "debug" ([("debug", [json|{"msg":"Adding {{ x }} + {{ y }}"}|]), ("name", [json|"Adding numbers"|])] <> taskAttrs) (taskVars)
  assertCheckingResults0 <- runTask playAttrs "assert" ([("assert", [json|{"that":["x == '42' and y == '21'","add_result['msg'] == 'Adding 42 + 21'"]}|]), ("name", [json|"Checking results"|])] <> taskAttrs) ([("add_result", debugAddingNumbers0)] <> taskVars)
  pure $ [debugAddingNumbers0] <> [assertCheckingResults0]

