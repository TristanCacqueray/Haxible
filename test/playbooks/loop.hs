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
main = runHaxible "inventory.yaml" "test/playbooks/loop.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|]), ("vars", [json|{"xs":[1,2,3]}|])] <> parentPlayAttrs
  let loop_ = [[json|"A"|], [json|"B"|], [json|"C"|]]
  debugCallTaskInALoop0 <- traverseLoop (\__haxible_loop_item ->  runTask playAttrs "debug" ([("debug", [json|{"msg":"loop {{ item }}"}|]), ("name", [json|"Call task in a loop"|])] <> taskAttrs) ([("item", __haxible_loop_item)] <> taskVars) )  loop_
  loop_ <- extractLoop <$> runTask playAttrs "debug" [("debug", [json|{"msg":"{{ xs }}"}|])] (taskVars)
  debugLoopVar0 <- traverseLoop (\__haxible_loop_item ->  runTask playAttrs "debug" ([("debug", [json|{"msg":"loop control {{ lvar }}"}|]), ("name", [json|"Loop var"|])] <> taskAttrs) ([("lvar", __haxible_loop_item)] <> taskVars) )  loop_
  debug0 <- runTask playAttrs "debug" ([("debug", [json|{"msg":"loop result is {{ loop_res }}"}|])] <> taskAttrs) ([("loop_res", debugCallTaskInALoop0)] <> taskVars)
  pure $ [debugCallTaskInALoop0] <> [debugLoopVar0] <> [debug0]

