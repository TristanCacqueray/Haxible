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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/adder.yaml" expect (playbook [] [])
  where expect = []

playbook :: Vars -> Vars -> AnsibleHaxl [Value]
playbook playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = ""
  resultsLocalhost0 <- playLocalhost0 playAttrs  localVars
  resultsLocalhost1 <- playLocalhost1 playAttrs  ([("answer", resultsLocalhost0 !! 0)] <> localVars)
  pure $ resultsLocalhost0 <> resultsLocalhost1

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"42"}|])]) localVars
  assert0 <- runTask src playAttrs defaultVars "assert" ([("assert", [json|{"that":["answer['msg'] == '42'"]}|])]) ([("answer", debug0)] <> localVars)
  pure $ [debug0] <> [assert0]

playLocalhost1 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost1 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsAdder0 <- roleAdder0 playAttrs  ([("x", [json|"{{ answer['msg'] }}"|]), ("y", [json|"21"|])] <> localVars)
  debug1 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Over!"}|])]) localVars
  pure $ resultsAdder0 <> [debug1]

roleAdder0 :: Vars -> Vars -> AnsibleHaxl [Value]
roleAdder0 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = [("adder_version", [json|"42 {{ adder_commit | default('HEAD') }}"|])]
      src = "test/playbooks/roles/adder"
  debugAddingNumbers0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Adding {{ x }} + {{ y }} with {{ adder_version }}"}|]), ("name", [json|"Adding numbers"|])]) localVars
  pure $ [debugAddingNumbers0]

