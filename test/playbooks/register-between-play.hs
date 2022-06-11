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
  where expect = []

playbook :: Vars -> Vars -> AnsibleHaxl [Value]
playbook playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = ""
  resultsLocalhost0 <- playLocalhost0 playAttrs  localVars
  resultsLocalhost1 <- playLocalhost1 playAttrs  ([("r1", resultsLocalhost0 !! 0)] <> localVars)
  pure $ resultsLocalhost0 <> resultsLocalhost1

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  stat0 <- runTask src playAttrs defaultVars "stat" ([("stat", [json|{"path":"/"}|])]) localVars
  pure $ [stat0]

playLocalhost1 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost1 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"r1 is {{ r1 }}"}|])]) localVars
  pure $ [debug0]

