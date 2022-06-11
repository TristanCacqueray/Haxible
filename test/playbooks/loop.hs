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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/loop.yaml" expect (playbook [] [])
  where expect = []

playbook :: Vars -> Vars -> AnsibleHaxl [Value]
playbook playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = ""
  resultsLocalhost0 <- playLocalhost0 playAttrs  localVars
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|]), ("vars", [json|{"xs":[1,2,3]}|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  let loop_ = [[json|"A"|], [json|"B"|], [json|"C"|]]
  let loopFun loop_item = do
        runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"loop {{ item }}"}|]), ("name", [json|"Call task in a loop"|])]) ([("item", loop_item)] <> localVars)
  debugCallTaskInALoop0 <- traverseLoop loopFun loop_
  loop_ <- extractLoop <$> runTask "" playAttrs defaultVars "debug" [("name", [json|"Resolving template {{ xs }}"|]), ("debug", [json|{"msg":"{{ xs }}"}|])] localVars
  let loopFun loop_item = do
        runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"loop control {{ lvar }}"}|]), ("name", [json|"Loop var"|])]) ([("lvar", loop_item)] <> localVars)
  debugLoopVar0 <- traverseLoop loopFun loop_
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"loop result is {{ loop_res }}"}|])]) ([("loop_res", debugCallTaskInALoop0)] <> localVars)
  pure $ [debugCallTaskInALoop0] <> [debugLoopVar0] <> [debug0]

