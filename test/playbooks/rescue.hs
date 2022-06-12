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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/rescue.yaml" expect (playbook [] [])
  where expect = [[json|{"changed":false,"msg":"rescue task"}|], [json|{"block_result":{"changed":false,"msg":"rescue task"},"changed":false}|]]

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
  resultsBlock0 <- tryRescue block0Main block0Rescue playAttrs (localVars <> defaultVars)
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"var":"block_result"}|])]) ([("block_result", resultsBlock0 !! 0)] <> localVars)
  pure $ resultsBlock0 <> [debug0]

block0Rescue :: Vars -> Vars -> AnsibleHaxl [Value]
block0Rescue playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks"
  debugRescueTask0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"rescue task"}|]), ("name", [json|"rescue task"|])]) localVars
  pure $ [debugRescueTask0]

block0Main :: Vars -> Vars -> AnsibleHaxl [Value]
block0Main playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks"
  commandBlockTask0 <- runTask src playAttrs defaultVars "command" ([("command", [json|"exit 1"|]), ("name", [json|"block task"|])]) localVars
  pure $ [commandBlockTask0]

