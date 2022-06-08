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
main = runHaxible "inventory.yaml" "test/playbooks/set-fact-when.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|])] <> parentPlayAttrs
  facts0 <- extractFact <$> runTask playAttrs "set_fact" [json|{"set_fact":{"x":42},"when":false}|] taskAttrs (taskVars)
  facts1 <- extractFact <$> runTask playAttrs "set_fact" [json|{"set_fact":{"x":41},"when":true}|] taskAttrs (taskVars)
  debug0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"x is {{ x }}"}}|] taskAttrs ([("x", facts1), ("x", facts0)] <> taskVars)
  pure $ [facts0] <> [facts1] <> [debug0]
