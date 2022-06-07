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
  debugCallTaskInALoop0 <- traverseLoop (\__haxible_loop_item ->  runTask playAttrs "debug" [json|{"debug":{"msg":"loop {{ item }}"},"name":"Call task in a loop"}|] ([("item", __haxible_loop_item)] <> baseEnv) )  [[json|"A"|], [json|"B"|], [json|"C"|]]
  debug0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"loop result is {{ loop_res }}"}}|] ([("loop_res", debugCallTaskInALoop0)] <> baseEnv)
  pure $ [debugCallTaskInALoop0] <> [debug0]

