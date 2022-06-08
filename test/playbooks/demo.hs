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
main = runHaxible "inventory.yaml" "test/playbooks/demo.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|])] <> parentPlayAttrs
  create_networkCreateNetwork0 <- runTask playAttrs "create_network" [json|{"create_network":{"name":"private"}}|] taskAttrs (taskVars)
  create_instanceCreateInstances0 <- traverseLoop (\__haxible_loop_item ->  runTask playAttrs "create_instance" [json|{"create_instance":{"name":"{{ item }}","network":"{{ network.uid }}"}}|] taskAttrs ([("item", __haxible_loop_item), ("network", create_networkCreateNetwork0)] <> taskVars) )  [[json|"backend"|], [json|"frontend"|], [json|"monitoring"|]]
  create_volumeCreateStorage0 <- runTask playAttrs "create_volume" [json|{"create_volume":{"name":"db"}}|] taskAttrs (taskVars)
  create_instanceCreateDatabase0 <- runTask playAttrs "create_instance" [json|{"create_instance":{"name":"database","network":"{{ network.uid }}","volume":"{{ storage.uid }}"}}|] taskAttrs ([("storage", create_volumeCreateStorage0), ("network", create_networkCreateNetwork0)] <> taskVars)
  create_objectCreateObject0 <- runTask playAttrs "create_object" [json|{"create_object":{"name":"standalone-object"}}|] taskAttrs (taskVars)
  create_objectCreateNetworkObject0 <- runTask playAttrs "create_object" [json|{"create_object":{"name":"network-{{ network.uid }}"}}|] taskAttrs ([("network", create_networkCreateNetwork0)] <> taskVars)
  pure $ [create_networkCreateNetwork0] <> [create_instanceCreateInstances0] <> [create_volumeCreateStorage0] <> [create_instanceCreateDatabase0] <> [create_objectCreateObject0] <> [create_objectCreateNetworkObject0]

