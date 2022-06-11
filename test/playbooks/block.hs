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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/block.yaml" expect (playbook [] [])
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
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  let when_ = True
  block0 <- if when_ then (block0 playAttrs  localVars) else pure [[json|{"changed":false,"skip_reason":"Conditional result was False"}|], [json|{"changed":false,"skip_reason":"Conditional result was False"}|]]
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"var":"block_result"}|])]) ([("block_result", block0 !! 1)] <> localVars)
  pure $ block0 <> [debug0]

block0 :: Vars -> Vars -> AnsibleHaxl [Value]
block0 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks"
  debugBlockTask0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"block task 1"}|]), ("name", [json|"block task"|])]) localVars
  debugBlockTask1 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"block task 2"}|]), ("name", [json|"block task"|])]) localVars
  pure $ [debugBlockTask0] <> [debugBlockTask1]

