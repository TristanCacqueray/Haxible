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
main = runHaxible "inventory.yaml" "test/playbooks/rescue.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|])] <> parentPlayAttrs
  block0 <- tryRescue (block0Main playAttrs) (block0Rescue playAttrs) (taskAttrs) (taskVars)
  debug0 <- runTask playAttrs "debug" [json|{"debug":{"var":"block_result"}}|] taskAttrs ([("block_result", block0 !! 0)] <> taskVars)
  pure $ block0 <> [debug0]

block0Rescue :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
block0Rescue parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  debugRescueTask0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"rescue task"}}|] taskAttrs (taskVars)
  pure $ [debugRescueTask0]

block0Main :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
block0Main parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  commandBlockTask0 <- runTask playAttrs "command" [json|{"command":"exit 1"}|] taskAttrs (taskVars)
  pure $ [commandBlockTask0]

