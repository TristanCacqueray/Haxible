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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/handlers.yaml" expect (playbook [] [])
  where expect = []

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
  command0 <- runTask src playAttrs defaultVars "command" ([("command", [json|"echo change config"|]), ("notify", [json|"Run Handler"|])]) localVars
  command1 <- runTask src playAttrs defaultVars "command" ([("command", [json|"echo noop"|]), ("changed_when", [json|false|]), ("notify", [json|"Run Other Handler"|])]) ([("_fake_TestPlaybooks", command0)] <> localVars)
  let res = [command0] <> [command1]
  playLocalhost0Handlers playAttrs res
  pure res

playLocalhost0Handlers :: Vars -> [Value] -> AnsibleHaxl ()
playLocalhost0Handlers playAttrs res = do
  notifyHandler playAttrs res "Run Handler" "debug" [("debug", [json|{"msg":"Running handler"}|]), ("name", [json|"Run Handler"|])]
  notifyHandler playAttrs res "Run Other Handler" "debug" [("debug", [json|{"msg":"KO, other handler should not run"}|]), ("name", [json|"Run Other Handler"|])]

