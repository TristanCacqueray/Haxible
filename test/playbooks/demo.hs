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
      src = ""
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|])] <> parentPlayAttrs
      src = "test/playbooks"
  create_networkCreateNetwork0 <- runTask src playAttrs "create_network" ([("create_network", [json|{"name":"private"}|]), ("name", [json|"Create network"|])] <> taskAttrs) (taskVars)
  let loop_ = [[json|"backend"|], [json|"frontend"|], [json|"monitoring"|]]
  let loopFun loop_item = do
        runTask src playAttrs "create_instance" ([("create_instance", [json|{"name":"{{ item }}","network":"{{ network.uid }}"}|]), ("name", [json|"Create instances"|])] <> taskAttrs) ([("item", loop_item), ("network", create_networkCreateNetwork0)] <> taskVars)
  create_instanceCreateInstances0 <- traverseLoop loopFun loop_
  create_volumeCreateStorage0 <- runTask src playAttrs "create_volume" ([("create_volume", [json|{"name":"db"}|]), ("name", [json|"Create storage"|])] <> taskAttrs) (taskVars)
  create_instanceCreateDatabase0 <- runTask src playAttrs "create_instance" ([("create_instance", [json|{"name":"database","network":"{{ network.uid }}","volume":"{{ storage.uid }}"}|]), ("name", [json|"Create database"|])] <> taskAttrs) ([("storage", create_volumeCreateStorage0), ("network", create_networkCreateNetwork0)] <> taskVars)
  create_objectCreateObject0 <- runTask src playAttrs "create_object" ([("create_object", [json|{"name":"standalone-object"}|]), ("name", [json|"Create object"|])] <> taskAttrs) (taskVars)
  create_objectCreateNetworkObject0 <- runTask src playAttrs "create_object" ([("create_object", [json|{"name":"network-{{ network.uid }}"}|]), ("name", [json|"Create network object"|])] <> taskAttrs) ([("network", create_networkCreateNetwork0)] <> taskVars)
  pure $ [create_networkCreateNetwork0] <> [create_instanceCreateInstances0] <> [create_volumeCreateStorage0] <> [create_instanceCreateDatabase0] <> [create_objectCreateObject0] <> [create_objectCreateNetworkObject0]

