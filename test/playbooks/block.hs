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
main = runHaxible "inventory.yaml" "test/playbooks/block.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
      src = ""
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|])] <> parentPlayAttrs
      src = "test/playbooks"
  let when_ = True
  block0 <- if when_ then (block0 playAttrs (taskAttrs) (taskVars)) else pure [[json|{"changed":false,"skip_reason":"Conditional result was False"}|], [json|{"changed":false,"skip_reason":"Conditional result was False"}|]]
  debug0 <- runTask src playAttrs "debug" ([("debug", [json|{"var":"block_result"}|])] <> taskAttrs) ([("block_result", block0 !! 1)] <> taskVars)
  pure $ block0 <> [debug0]

block0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
block0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
      src = "test/playbooks"
  debugBlockTask0 <- runTask src playAttrs "debug" ([("debug", [json|{"msg":"block task 1"}|]), ("name", [json|"block task"|])] <> taskAttrs) (taskVars)
  debugBlockTask1 <- runTask src playAttrs "debug" ([("debug", [json|{"msg":"block task 2"}|]), ("name", [json|"block task"|])] <> taskAttrs) (taskVars)
  pure $ [debugBlockTask0] <> [debugBlockTask1]

