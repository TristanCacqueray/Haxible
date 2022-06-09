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
main = runHaxible "inventory.yaml" "test/playbooks/simple.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
      src = ""
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  resultsZuulExecutor0 <- playZuulExecutor0 playAttrs (taskAttrs) (taskVars)
  resultsNodepoolLauncher0 <- playNodepoolLauncher0 playAttrs (taskAttrs) (taskVars)
  resultsLocalhost1 <- playLocalhost1 playAttrs (taskAttrs) ([("etc", resultsLocalhost0 !! 0)] <> taskVars)
  pure $ resultsLocalhost0 <> resultsZuulExecutor0 <> resultsNodepoolLauncher0 <> resultsLocalhost1

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|])] <> parentPlayAttrs
      src = "test/playbooks"
  stat0 <- runTask src playAttrs "stat" ([("stat", [json|{"path":"/etc"}|])] <> taskAttrs) (taskVars)
  command0 <- runTask src playAttrs "command" ([("command", [json|"echo etc exist: {{ etc.stat.exists }}"|])] <> taskAttrs) ([("etc", stat0), ("_fake_Etc", stat0)] <> taskVars)
  pure $ [stat0] <> [command0]

playZuulExecutor0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playZuulExecutor0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"zuul_executor"|])] <> parentPlayAttrs
      src = "test/playbooks"
  file0 <- runTask src playAttrs "file" ([("file", [json|{"path":"/tmp/zuul","state":"directory"}|])] <> taskAttrs) (taskVars)
  command1 <- runTask src playAttrs "command" ([("command", [json|"echo Starting executor -d /tmp/zuul"|])] <> taskAttrs) ([("zuuldir", file0), ("_fake_TmpZuul", file0)] <> taskVars)
  pure $ [file0] <> [command1]

playNodepoolLauncher0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playNodepoolLauncher0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"nodepool_launcher"|])] <> parentPlayAttrs
      src = "test/playbooks"
  file1 <- runTask src playAttrs "file" ([("file", [json|{"path":"/tmp/nodepool","state":"directory"}|])] <> taskAttrs) (taskVars)
  command2 <- runTask src playAttrs "command" ([("command", [json|"echo Starting scheduler -d /tmp/nodepool"|])] <> taskAttrs) ([("_fake_TmpNodepool", file1)] <> taskVars)
  debug0 <- runTask src playAttrs "debug" ([("debug", [json|null|])] <> taskAttrs) (taskVars)
  pure $ [file1] <> [command2] <> [debug0]

playLocalhost1 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost1 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|])] <> parentPlayAttrs
      src = "test/playbooks"
  debug1 <- runTask src playAttrs "debug" ([("debug", [json|{"msg":"etc stats is {{ etc }}"}|])] <> taskAttrs) (taskVars)
  pure $ [debug1]

