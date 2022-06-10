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
main = runHaxible "inventory.yaml" "test/playbooks/template.yaml" (playbook [] [] [])

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
  facts0 <- extractFact <$> runTask src playAttrs "set_fact" ([("set_fact", [json|{"greet":"Haxible"}|])] <> taskAttrs) (taskVars)
  template0 <- runTask src playAttrs "template" ([("template", [json|{"dest":"/tmp/haxible.txt","src":"file.j2"}|])] <> taskAttrs) ([("greet", facts0)] <> taskVars)
  debug0 <- runTask src playAttrs "debug" ([("debug", [json|{"var":"res"}|])] <> taskAttrs) ([("res", template0), ("_fake_TmpHaxibleTxt", template0)] <> taskVars)
  pure $ [facts0] <> [template0] <> [debug0]

