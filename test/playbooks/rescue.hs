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
  block0 <- tryRescue block0Main block0Rescue ([] <> playAttrs) ([] <> [] <> baseEnv)
  debug0 <- runTask playAttrs "debug" [json|{"debug":{"var":"block_result"}}|] ([("block_result", block0 !! 0)] <> baseEnv)
  pure $ block0 <> [debug0]

block0Rescue :: Vars -> Vars -> AnsibleHaxl [Value]
block0Rescue playAttrs baseEnv = do
  debugRescueTask0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"rescue task"},"name":"rescue task"}|] ([] <> baseEnv)
  pure $ [debugRescueTask0]

block0Main :: Vars -> Vars -> AnsibleHaxl [Value]
block0Main playAttrs baseEnv = do
  commandBlockTask0 <- runTask playAttrs "command" [json|{"command":"exit 1","name":"block task"}|] ([] <> baseEnv)
  pure $ [commandBlockTask0]

