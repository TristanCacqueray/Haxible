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
main = runHaxible "inventory.yaml" "test/playbooks/register-between-play.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
      src = ""
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  resultsLocalhost1 <- playLocalhost1 playAttrs (taskAttrs) ([("r1", resultsLocalhost0 !! 0)] <> taskVars)
  pure $ resultsLocalhost0 <> resultsLocalhost1

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|])] <> parentPlayAttrs
      src = "test/playbooks"
  stat0 <- runTask src playAttrs "stat" ([("stat", [json|{"path":"/"}|])] <> taskAttrs) (taskVars)
  pure $ [stat0]

playLocalhost1 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost1 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|])] <> parentPlayAttrs
      src = "test/playbooks"
  debug0 <- runTask src playAttrs "debug" ([("debug", [json|{"msg":"r1 is {{ r1 }}"}|])] <> taskAttrs) (taskVars)
  pure $ [debug0]

