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
main = runHaxible "inventory.yaml" "test/playbooks/command.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
      src = ""
  resultsLocalhosts0 <- playLocalhosts0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsLocalhosts0

playLocalhosts0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhosts0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhosts"|])] <> parentPlayAttrs
      src = "test/playbooks"
  command0 <- runTask src playAttrs "command" ([("command", [json|"echo 1"|])] <> taskAttrs) (taskVars)
  command1 <- runTask src playAttrs "command" ([("command", [json|"echo 2"|])] <> taskAttrs) ([("_fake_TestPlaybooks", command0)] <> taskVars)
  pure $ [command0] <> [command1]

