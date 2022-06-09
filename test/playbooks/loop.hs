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
      src = ""
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|]), ("vars", [json|{"xs":[1,2,3]}|])] <> parentPlayAttrs
      src = "test/playbooks"
  let loop_ = [[json|"A"|], [json|"B"|], [json|"C"|]]
  let loopFun loop_item = do
        runTask src playAttrs "debug" ([("debug", [json|{"msg":"loop {{ item }}"}|]), ("name", [json|"Call task in a loop"|])] <> taskAttrs) ([("item", loop_item)] <> taskVars)
  debugCallTaskInALoop0 <- traverseLoop loopFun loop_
  loop_ <- extractLoop <$> runTask "" playAttrs "debug" [("name", [json|"Resolving template {{ xs }}"|]), ("debug", [json|{"msg":"{{ xs }}"}|])] (taskVars)
  let loopFun loop_item = do
        runTask src playAttrs "debug" ([("debug", [json|{"msg":"loop control {{ lvar }}"}|]), ("name", [json|"Loop var"|])] <> taskAttrs) ([("lvar", loop_item)] <> taskVars)
  debugLoopVar0 <- traverseLoop loopFun loop_
  debug0 <- runTask src playAttrs "debug" ([("debug", [json|{"msg":"loop result is {{ loop_res }}"}|])] <> taskAttrs) ([("loop_res", debugCallTaskInALoop0)] <> taskVars)
  pure $ [debugCallTaskInALoop0] <> [debugLoopVar0] <> [debug0]

