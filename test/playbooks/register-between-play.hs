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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/register-between-play.yaml" expect (playbook [] [])
  where expect = [[json|{"changed":false,"msg":"r1 value"}|], [json|{"changed":false,"msg":"r1 is {'changed': False, 'msg': 'r1 value'}"}|]]

playbook :: Vars -> Vars -> AnsibleHaxl [Value]
playbook playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = ""
  resultsPlayLocalhost0 <- playLocalhost0 playAttrs (localVars <> defaultVars)
  resultsPlayLocalhost1 <- playLocalhost1 playAttrs ([("r1", resultsPlayLocalhost0 !! 0)] <> localVars <> defaultVars)
  pure $ resultsPlayLocalhost0 <> resultsPlayLocalhost1

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"r1 value"}|])]) localVars
  pure $ [debug0]

playLocalhost1 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost1 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  debug1 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"r1 is {{ r1 }}"}|])]) localVars
  pure $ [debug1]

