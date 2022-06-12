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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/demo.yaml" expect (playbook [] [])
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
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  create_networkCreateNetwork0 <- runTask src playAttrs defaultVars "create_network" ([("create_network", [json|{"name":"private"}|]), ("name", [json|"Create network"|])]) localVars
  let loop_ = [[json|"backend"|], [json|"frontend"|], [json|"monitoring"|]]
  let loopFun loop_item = do
        runTask src playAttrs defaultVars "create_instance" ([("create_instance", [json|{"name":"{{ item }}","network":"{{ network.uid }}"}|]), ("name", [json|"Create instances"|])]) ([("item", loop_item), ("network", create_networkCreateNetwork0)] <> localVars)
  create_instanceCreateInstances0 <- traverseLoop loopFun loop_
  create_volumeCreateStorage0 <- runTask src playAttrs defaultVars "create_volume" ([("create_volume", [json|{"name":"db"}|]), ("name", [json|"Create storage"|])]) localVars
  create_instanceCreateDatabase0 <- runTask src playAttrs defaultVars "create_instance" ([("create_instance", [json|{"name":"database","network":"{{ network.uid }}","volume":"{{ storage.uid }}"}|]), ("name", [json|"Create database"|])]) ([("storage", create_volumeCreateStorage0), ("network", create_networkCreateNetwork0)] <> localVars)
  create_objectCreateObject0 <- runTask src playAttrs defaultVars "create_object" ([("create_object", [json|{"name":"standalone-object"}|]), ("name", [json|"Create object"|])]) localVars
  create_objectCreateNetworkObject0 <- runTask src playAttrs defaultVars "create_object" ([("create_object", [json|{"name":"network-{{ network.uid }}"}|]), ("name", [json|"Create network object"|])]) ([("network", create_networkCreateNetwork0)] <> localVars)
  pure $ [create_networkCreateNetwork0] <> [create_instanceCreateInstances0] <> [create_volumeCreateStorage0] <> [create_instanceCreateDatabase0] <> [create_objectCreateObject0] <> [create_objectCreateNetworkObject0]

