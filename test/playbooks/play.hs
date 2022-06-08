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
main = runHaxible "inventory.yaml" "test/playbooks/play.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|]), ("vars", [json|{"x":"42","y":"21"}|])] <> parentPlayAttrs
  debug0 <- runTask playAttrs "debug" ([("debug", [json|{"msg":"a pre task"}|])] <> taskAttrs) (taskVars)
  resultsAdder0 <- roleAdder0 playAttrs (taskAttrs) (taskVars)
  debug1 <- runTask playAttrs "debug" ([("debug", [json|{"msg":"a task"}|])] <> taskAttrs) (taskVars)
  debug2 <- runTask playAttrs "debug" ([("debug", [json|{"msg":"a post task"}|])] <> taskAttrs) (taskVars)
  pure $ [debug0] <> resultsAdder0 <> [debug1] <> [debug2]

roleAdder0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
roleAdder0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  debugAddingNumbers0 <- runTask playAttrs "debug" ([("debug", [json|{"msg":"Adding {{ x }} + {{ y }}"}|])] <> taskAttrs) (taskVars)
  assertCheckingResults0 <- runTask playAttrs "assert" ([("assert", [json|{"that":["x == '42' and y == '21'","add_result['msg'] == 'Adding 42 + 21'"]}|])] <> taskAttrs) ([("add_result", debugAddingNumbers0)] <> taskVars)
  pure $ [debugAddingNumbers0] <> [assertCheckingResults0]

