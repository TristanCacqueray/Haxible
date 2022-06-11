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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/include-vars.yaml" expect (playbook [] [])
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
  let playAttrs = [("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  vars0 <- runTask src playAttrs defaultVars "include_vars" ([("include_vars", [json|"./roles/adder/defaults/main.yaml"|])]) localVars
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"adder_version = {{ adder_version }}"}|])]) ([("_fakev_TestPlaybooks", vars0)] <> localVars)
  pure $ [vars0] <> [debug0]

