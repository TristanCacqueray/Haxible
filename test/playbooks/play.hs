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
  resultsLocalhost0 <- playLocalhost0 ([("hosts", [json|"localhost"|]), ("vars", [json|{"x":"42","y":"21"}|])] <> playAttrs) ([] <> [] <> baseEnv)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs baseEnv = do
  debug0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"a pre task"}}|] ([] <> baseEnv)
  roleAdder0 <- roleAdder ([] <> playAttrs) ([] <> [] <> baseEnv)
  debug1 <- runTask playAttrs "debug" [json|{"debug":{"msg":"a task"}}|] ([] <> baseEnv)
  debug2 <- runTask playAttrs "debug" [json|{"debug":{"msg":"a post task"}}|] ([] <> baseEnv)
  pure $ [debug0] <> roleAdder0 <> [debug1] <> [debug2]

roleAdder :: Vars -> Vars -> AnsibleHaxl [Value]
roleAdder playAttrs baseEnv = do
  debugAddingNumbers0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"Adding {{ x }} + {{ y }}"},"name":"Adding numbers"}|] ([] <> baseEnv)
  assertCheckingResults0 <- runTask playAttrs "assert" [json|{"assert":{"that":["x == '42' and y == '21'","add_result['msg'] == 'Adding 42 + 21'"]},"name":"Checking results"}|] ([("add_result", debugAddingNumbers0)] <> baseEnv)
  pure $ [debugAddingNumbers0] <> [assertCheckingResults0]

