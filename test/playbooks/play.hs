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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/play.yaml" expect (playbook [] [])
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
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|]), ("vars", [json|{"x":"42","y":"21"}|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"a pre task"}|])]) localVars
  resultsRoleAdder0 <- roleAdder0 playAttrs (localVars <> defaultVars)
  debug1 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"a task"}|])]) localVars
  debug2 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"a post task"}|])]) localVars
  pure $ [debug0] <> resultsRoleAdder0 <> [debug1] <> [debug2]

roleAdder0 :: Vars -> Vars -> AnsibleHaxl [Value]
roleAdder0 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = [("adder_version", [json|"42 {{ adder_commit | default('HEAD') }}"|])]
      src = "test/playbooks/roles/adder"
  debugAddingNumbers0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Adding {{ x }} + {{ y }} with {{ adder_version }}"}|]), ("name", [json|"Adding numbers"|])]) localVars
  pure $ [debugAddingNumbers0]

