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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/template.yaml" expect (playbook [] [])
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
  let playAttrs = [("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  facts0 <- extractFact <$> runTask src playAttrs defaultVars "set_fact" ([("set_fact", [json|{"greet":"Haxible"}|])]) localVars
  template0 <- runTask src playAttrs defaultVars "template" ([("template", [json|{"dest":"/tmp/haxible.txt","src":"file.j2"}|])]) ([("greet", facts0)] <> localVars)
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"var":"res"}|])]) ([("res", template0), ("_fake_TmpHaxibleTxt", template0)] <> localVars)
  pure $ [facts0] <> [template0] <> [debug0]

