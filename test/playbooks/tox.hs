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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/tox.yaml" expect (playbook [] [])
  where expect = [[json|{"changed":false,"msg":"task 1"}|], [json|{"changed":false,"msg":"task 2"}|], [json|{"ansible_facts":{"pip_command":"/opt/bin/pip"},"changed":false}|], [json|{"changed":false,"msg":"Running /opt/bin/pip"}|]]

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
  resultsRoleEnsureTox0 <- roleEnsureTox0 playAttrs (localVars <> defaultVars)
  pure $ resultsRoleEnsureTox0

roleEnsureTox0 :: Vars -> Vars -> AnsibleHaxl [Value]
roleEnsureTox0 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks/roles/ensure-tox"
  resultsRoleEnsurePip0 <- roleEnsurePip0 playAttrs (localVars <> defaultVars)
  resultsBlock1 <- block1 playAttrs ([("pip_command", resultsRoleEnsurePip0 !! 2)] <> localVars <> defaultVars)
  pure $ resultsRoleEnsurePip0 <> resultsBlock1

block1 :: Vars -> Vars -> AnsibleHaxl [Value]
block1 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks/roles/ensure-tox"
  debug2 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Running {{ pip_command }}"}|])]) localVars
  pure $ [debug2]

roleEnsurePip0 :: Vars -> Vars -> AnsibleHaxl [Value]
roleEnsurePip0 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks/roles/ensure-pip"
  resultsBlock0 <- block0 playAttrs (localVars <> defaultVars)
  facts0 <- runTask src playAttrs defaultVars "set_fact" ([("set_fact", [json|{"cacheable":true,"pip_command":"/opt/bin/pip"}|])]) localVars
  pure $ resultsBlock0 <> [facts0]

block0 :: Vars -> Vars -> AnsibleHaxl [Value]
block0 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks/roles/ensure-pip"
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"task 1"}|])]) localVars
  debug1 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"task 2"}|])]) localVars
  pure $ [debug0] <> [debug1]

