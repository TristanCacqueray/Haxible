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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/loop-when.yaml" expect (playbook [] [])
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
  let playAttrs = [("hosts", [json|"localhost"|]), ("vars", [json|{"xs":[{"enabled":true,"msg":"Hello"},{"enabled":false,"msg":"Exit"}]}|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  loop_ <- extractLoop <$> runTask "" playAttrs defaultVars "debug" [("name", [json|"Resolving template {{ xs }}"|]), ("debug", [json|{"msg":"{{ xs }}"}|])] localVars
  let loopFun loop_item = do
        when_ <- extractWhen <$> runTask "" playAttrs defaultVars "debug" [("name", [json|"Resolving template {{ loop_item.enabled }}"|]), ("debug", [json|{"msg":"{{ loop_item.enabled }}"}|])] ([("loop_item", loop_item)] <> localVars)
        if when_ then (runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"{{ loop_item.msg }}"}|])]) ([("loop_item", loop_item)] <> localVars)) else pure [json|{"changed":false,"skip_reason":"Conditional result was False"}|]
  debug0 <- traverseLoop loopFun loop_
  pure $ [debug0]

