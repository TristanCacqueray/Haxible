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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/set-fact-when.yaml" expect (playbook [] [])
  where expect = [[json|{"changed":false,"skip_reason":"Conditional result was False"}|], [json|{"ansible_facts":{"x":41},"changed":false}|], [json|{"changed":false,"msg":"x is 41"}|]]

playbook :: Vars -> Vars -> AnsibleHaxl [Value]
playbook playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = ""
  resultsPlayLocalhost0 <- playLocalhost0 playAttrs (localVars <> defaultVars)
  pure $ resultsPlayLocalhost0

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  let when_ = False
  facts0 <- if when_ then (runTask src playAttrs defaultVars "set_fact" ([("set_fact", [json|{"x":42}|])]) localVars) else pure [json|{"changed":false,"skip_reason":"Conditional result was False"}|]
  let when_ = True
  facts1 <- if when_ then (runTask src playAttrs defaultVars "set_fact" ([("set_fact", [json|{"x":41}|])]) localVars) else pure [json|{"changed":false,"skip_reason":"Conditional result was False"}|]
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"x is {{ x }}"}|])]) ([("x", facts1), ("x", facts0)] <> localVars)
  pure $ [facts0] <> [facts1] <> [debug0]

