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
main = runHaxible "inventory.yaml" (playbook [] [])

playbook :: Vars -> Vars -> AnsibleHaxl [Value]
playbook playAttrs baseEnv = do
  resultsLocalhost0 <- playLocalhost0 ([("hosts", [json|"localhost"|]), ("vars", [json|{"play_var":"play-var"}|])] <> playAttrs) ([] <> [] <> baseEnv)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs baseEnv = do
  factsSimpleFact0 <- extractFact <$> runTask playAttrs "set_fact" [json|{"set_fact":{"fact_var":"{{ play_var }}"}}|] ([] <> baseEnv)
  factsExtraFact0 <- extractFact <$> runTask playAttrs "set_fact" [json|{"set_fact":{"cacheable":true,"extra_var":"extra-{{ fact_var }}"}}|] ([("fact_var", factsSimpleFact0)] <> baseEnv)
  debug0 <- runTask playAttrs "debug" [json|{"debug":{"var":"extra_var"}}|] ([("extra_var", factsExtraFact0)] <> baseEnv)
  pure $ [factsSimpleFact0] <> [factsExtraFact0] <> [debug0]

