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
main = runHaxible "inventory.yaml" "test/playbooks/gather-facts.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
      src = ""
  resultsAll0 <- playAll0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsAll0

playAll0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playAll0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"all"|])] <> parentPlayAttrs
      src = "test/playbooks"
  debug0 <- runTask src playAttrs "debug" ([("debug", [json|"var=ansible_processor_cores"|])] <> taskAttrs) (taskVars)
  pure $ [debug0]

