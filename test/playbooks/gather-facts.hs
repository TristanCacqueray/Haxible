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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/gather-facts.yaml" expect (playbook [] [])
  where expect = []

playbook :: Vars -> Vars -> AnsibleHaxl [Value]
playbook playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = ""
  resultsPlayAll0 <- playAll0 playAttrs (localVars <> defaultVars)
  pure $ resultsPlayAll0

playAll0 :: Vars -> Vars -> AnsibleHaxl [Value]
playAll0 playAttrs' localVars = do
  let playAttrs = [("hosts", [json|"all"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|"var=ansible_processor_cores"|])]) localVars
  pure $ [debug0]

