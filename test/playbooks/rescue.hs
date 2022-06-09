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
      src = ""
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|])] <> parentPlayAttrs
      src = "test/playbooks"
  block0 <- tryRescue (block0Main playAttrs) (block0Rescue playAttrs) (taskAttrs) (taskVars)
  debug0 <- runTask src playAttrs "debug" ([("debug", [json|{"var":"block_result"}|])] <> taskAttrs) ([("block_result", block0 !! 0)] <> taskVars)
  pure $ block0 <> [debug0]

block0Rescue :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
block0Rescue parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
      src = "test/playbooks"
  debugRescueTask0 <- runTask src playAttrs "debug" ([("debug", [json|{"msg":"rescue task"}|]), ("name", [json|"rescue task"|])] <> taskAttrs) (taskVars)
  pure $ [debugRescueTask0]

block0Main :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
block0Main parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
      src = "test/playbooks"
  commandBlockTask0 <- runTask src playAttrs "command" ([("command", [json|"exit 1"|]), ("name", [json|"block task"|])] <> taskAttrs) (taskVars)
  pure $ [commandBlockTask0]

