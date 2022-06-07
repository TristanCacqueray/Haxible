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
main = runHaxible "inventory.yaml" "test/playbooks/register-between-play.yaml" (playbook [] [])

playbook :: Vars -> Vars -> AnsibleHaxl [Value]
playbook playAttrs baseEnv = do
  resultsLocalhost0 <- playLocalhost0 ([("hosts", [json|"localhost"|])] <> playAttrs) ([] <> [] <> baseEnv)
  resultsLocalhost1 <- playLocalhost1 ([("hosts", [json|"localhost"|])] <> playAttrs) ([("r1", resultsLocalhost0 !! 0)] <> [] <> baseEnv)
  pure $ resultsLocalhost0 <> resultsLocalhost1

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs baseEnv = do
  stat0 <- runTask playAttrs "stat" [json|{"stat":{"path":"/"}}|] ([] <> baseEnv)
  pure $ [stat0]

playLocalhost1 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost1 playAttrs baseEnv = do
  debug0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"r1 is {{ r1 }}"}}|] ([] <> baseEnv)
  pure $ [debug0]

