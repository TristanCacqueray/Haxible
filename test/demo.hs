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
main = runHaxible "inventory.yaml" (playbook [] [])

playbook :: Vars -> Vars -> AnsibleHaxl [Value]
playbook playAttrs baseEnv = do
  resultsLocalhost0 <- playLocalhost0 ([("hosts", [json|"localhost"|])] <> playAttrs) ([] <> [] <> baseEnv)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs baseEnv = do
  create_networkCreateNetwork0 <- runTask playAttrs "create_network" [json|{"create_network":{"name":"private"},"name":"Create network"}|] ([] <> baseEnv)
  create_instanceCreateInstances0 <- traverseLoop (\__haxible_loop_item ->  runTask playAttrs "create_instance" [json|{"create_instance":{"name":"{{ item }}","network":"{{ network.uid }}"},"name":"Create instances"}|] ([("item", __haxible_loop_item), ("network", create_networkCreateNetwork0)] <> baseEnv) )  [[json|"backend"|], [json|"frontend"|], [json|"monitoring"|]]
  create_storageCreateStorage0 <- runTask playAttrs "create_storage" [json|{"create_storage":{"name":"db"},"name":"Create storage"}|] ([] <> baseEnv)
  create_instanceCreateDatabase0 <- runTask playAttrs "create_instance" [json|{"create_instance":{"name":"database","network":"{{ network.uid }}","volume":"{{ storage.uid }}"},"name":"Create database"}|] ([("storage", create_storageCreateStorage0), ("network", create_networkCreateNetwork0)] <> baseEnv)
  create_objectCreateObject0 <- runTask playAttrs "create_object" [json|{"create_object":{"name":"standalone-object"},"name":"Create object"}|] ([] <> baseEnv)
  create_objectCreateNetworkObject0 <- runTask playAttrs "create_object" [json|{"create_object":{"name":"network-{{ network.uid }}"},"name":"Create network object"}|] ([("network", create_networkCreateNetwork0)] <> baseEnv)
  roleContainerService0 <- roleContainerService ([] <> playAttrs) ([] <> [("image_name", [json|"test"|]), ("image_tag", [json|"latest"|]), ("runtime_arg", [json|"--log-level debug"|])] <> baseEnv)
  pure $ [create_networkCreateNetwork0] <> [create_instanceCreateInstances0] <> [create_storageCreateStorage0] <> [create_instanceCreateDatabase0] <> [create_objectCreateObject0] <> [create_objectCreateNetworkObject0] <> roleContainerService0

roleContainerService :: Vars -> Vars -> AnsibleHaxl [Value]
roleContainerService playAttrs baseEnv = do
  include_vars0 <- runTask playAttrs "include_vars" [json|{"include_vars":"redhat.yaml"}|] ([] <> baseEnv)
  set_fact0 <- runTask playAttrs "set_fact" [json|{"set_fact":{"command":"{{ runtime }} run {{ image_name }}:{{ image_tag }}"},"vars":{"image_name":"ubi8"}}|] ([] <> baseEnv)
  debug0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"Running {{ command }}"}}|] ([] <> baseEnv)
  pure $ [include_vars0] <> [set_fact0] <> [debug0]

