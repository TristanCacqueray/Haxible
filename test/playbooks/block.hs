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
  block0 <- block0 ([] <> playAttrs) ([] <> [] <> baseEnv)
  debug0 <- runTask playAttrs "debug" [json|{"debug":{"var":"block_result"}}|] ([("block_result", block0 !! 1)] <> baseEnv)
  pure $ block0 <> [debug0]

block0 :: Vars -> Vars -> AnsibleHaxl [Value]
block0 playAttrs baseEnv = do
  debugBlockTask0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"block task 1"},"name":"block task"}|] ([] <> baseEnv)
  debugBlockTask1 <- runTask playAttrs "debug" [json|{"debug":{"msg":"block task 2"},"name":"block task"}|] ([] <> baseEnv)
  pure $ [debugBlockTask0] <> [debugBlockTask1]

