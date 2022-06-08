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
main = runHaxible "inventory.yaml" "test/playbooks/facts.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|]), ("vars", [json|{"play_var":"play-var"}|])] <> parentPlayAttrs
  factsSimpleFact0 <- extractFact <$> runTask playAttrs "set_fact" ([("set_fact", [json|{"fact_var":"{{ play_var }}"}|]), ("name", [json|"Simple fact"|])] <> taskAttrs) (taskVars)
  factsExtraFact0 <- extractFact <$> runTask playAttrs "set_fact" ([("set_fact", [json|{"cacheable":true,"extra_var":"extra-{{ fact_var }}"}|]), ("name", [json|"Extra fact"|])] <> taskAttrs) ([("fact_var", factsSimpleFact0)] <> taskVars)
  debug0 <- runTask playAttrs "debug" ([("debug", [json|{"var":"extra_var"}|])] <> taskAttrs) ([("extra_var", factsExtraFact0)] <> taskVars)
  pure $ [factsSimpleFact0] <> [factsExtraFact0] <> [debug0]

