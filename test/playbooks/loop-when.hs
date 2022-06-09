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
main = runHaxible "inventory.yaml" "test/playbooks/loop-when.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
      src = ""
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|]), ("vars", [json|{"xs":[{"enabled":true,"msg":"Hello"},{"enabled":false,"msg":"Exit"}]}|])] <> parentPlayAttrs
      src = "test/playbooks"
  loop_ <- extractLoop <$> runTask "" playAttrs "debug" [("name", [json|"Resolving template {{ xs }}"|]), ("debug", [json|{"msg":"{{ xs }}"}|])] (taskVars)
  let loopFun loop_item = do
        when_ <- extractWhen <$> runTask "" playAttrs "debug" [("name", [json|"Resolving template {{ loop_item.enabled }}"|]), ("debug", [json|{"msg":"{{ loop_item.enabled }}"}|])] ([("loop_item", loop_item)] <> taskVars)
        if when_ then (runTask src playAttrs "debug" ([("debug", [json|{"msg":"{{ loop_item.msg }}"}|])] <> taskAttrs) ([("loop_item", loop_item)] <> taskVars)) else pure [json|{"changed":false,"skip_reason":"Conditional result was False"}|]
  debug0 <- traverseLoop loopFun loop_
  pure $ [debug0]

