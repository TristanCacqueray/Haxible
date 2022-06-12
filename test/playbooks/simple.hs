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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/simple.yaml" expect (playbook [] [])
  where expect = []

playbook :: Vars -> Vars -> AnsibleHaxl [Value]
playbook playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = ""
  resultsPlayLocalhost0 <- playLocalhost0 playAttrs (localVars <> defaultVars)
  resultsPlayZuulExecutor0 <- playZuulExecutor0 playAttrs (localVars <> defaultVars)
  resultsPlayNodepoolLauncher0 <- playNodepoolLauncher0 playAttrs (localVars <> defaultVars)
  resultsPlayLocalhost1 <- playLocalhost1 playAttrs ([("etc", resultsPlayLocalhost0 !! 0)] <> localVars <> defaultVars)
  pure $ resultsPlayLocalhost0 <> resultsPlayZuulExecutor0 <> resultsPlayNodepoolLauncher0 <> resultsPlayLocalhost1

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  stat0 <- runTask src playAttrs defaultVars "stat" ([("stat", [json|{"path":"/etc"}|])]) localVars
  command0 <- runTask src playAttrs defaultVars "command" ([("command", [json|"echo etc exist: {{ etc.stat.exists }}"|])]) ([("etc", stat0), ("_fake_Etc", stat0)] <> localVars)
  pure $ [stat0] <> [command0]

playZuulExecutor0 :: Vars -> Vars -> AnsibleHaxl [Value]
playZuulExecutor0 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"zuul_executor"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  file0 <- runTask src playAttrs defaultVars "file" ([("file", [json|{"path":"/tmp/zuul","state":"directory"}|])]) localVars
  command1 <- runTask src playAttrs defaultVars "command" ([("command", [json|"echo Starting executor -d /tmp/zuul"|])]) ([("zuuldir", file0), ("_fake_TmpZuul", file0)] <> localVars)
  pure $ [file0] <> [command1]

playNodepoolLauncher0 :: Vars -> Vars -> AnsibleHaxl [Value]
playNodepoolLauncher0 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"nodepool_launcher"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  file1 <- runTask src playAttrs defaultVars "file" ([("file", [json|{"path":"/tmp/nodepool","state":"directory"}|])]) localVars
  command2 <- runTask src playAttrs defaultVars "command" ([("command", [json|"echo Starting scheduler -d /tmp/nodepool"|])]) ([("_fake_TmpNodepool", file1)] <> localVars)
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|null|])]) localVars
  pure $ [file1] <> [command2] <> [debug0]

playLocalhost1 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost1 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  debug1 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"etc stats is {{ etc }}"}|])]) localVars
  pure $ [debug1]

