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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/facts.yaml" expect (playbook [] [])
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
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|]), ("vars", [json|{"play_var":"play-var"}|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  factsSimpleFact0 <- extractFact <$> runTask src playAttrs defaultVars "set_fact" ([("set_fact", [json|{"fact_var":"{{ play_var }}"}|]), ("name", [json|"Simple fact"|])]) localVars
  factsExtraFact0 <- extractFact <$> runTask src playAttrs defaultVars "set_fact" ([("set_fact", [json|{"cacheable":true,"extra_var":"extra-{{ fact_var }}"}|]), ("name", [json|"Extra fact"|])]) ([("fact_var", factsSimpleFact0)] <> localVars)
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"var":"extra_var"}|])]) ([("extra_var", factsExtraFact0)] <> localVars)
  pure $ [factsSimpleFact0] <> [factsExtraFact0] <> [debug0]

