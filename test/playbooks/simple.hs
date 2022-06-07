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
main = runHaxible "inventory.yaml" "test/playbooks/simple.yaml" (playbook [] [])

playbook :: Vars -> Vars -> AnsibleHaxl [Value]
playbook playAttrs baseEnv = do
  resultsLocalhost0 <- playLocalhost0 ([("hosts", [json|"localhost"|])] <> playAttrs) ([] <> [] <> baseEnv)
  resultsZuulExecutor0 <- playZuulExecutor0 ([("hosts", [json|"zuul_executor"|])] <> playAttrs) ([] <> [] <> baseEnv)
  resultsNodepoolLauncher0 <- playNodepoolLauncher0 ([("hosts", [json|"nodepool_launcher"|])] <> playAttrs) ([] <> [] <> baseEnv)
  resultsLocalhost1 <- playLocalhost1 ([("hosts", [json|"localhost"|])] <> playAttrs) ([("etc", resultsLocalhost0 !! 0)] <> [] <> baseEnv)
  pure $ resultsLocalhost0 <> resultsZuulExecutor0 <> resultsNodepoolLauncher0 <> resultsLocalhost1

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs baseEnv = do
  stat0 <- runTask playAttrs "stat" [json|{"stat":{"path":"/etc"}}|] ([] <> baseEnv)
  command0 <- runTask playAttrs "command" [json|{"command":"echo etc exist: {{ etc.stat.exists }}"}|] ([("etc", stat0), ("_etc", stat0)] <> baseEnv)
  pure $ [stat0] <> [command0]

playZuulExecutor0 :: Vars -> Vars -> AnsibleHaxl [Value]
playZuulExecutor0 playAttrs baseEnv = do
  file0 <- runTask playAttrs "file" [json|{"file":{"path":"/tmp/zuul","state":"directory"}}|] ([] <> baseEnv)
  command1 <- runTask playAttrs "command" [json|{"command":"echo Starting executor -d /tmp/zuul"}|] ([("zuuldir", file0), ("_tmp_zuul", file0)] <> baseEnv)
  pure $ [file0] <> [command1]

playNodepoolLauncher0 :: Vars -> Vars -> AnsibleHaxl [Value]
playNodepoolLauncher0 playAttrs baseEnv = do
  file1 <- runTask playAttrs "file" [json|{"file":{"path":"/tmp/nodepool","state":"directory"}}|] ([] <> baseEnv)
  command2 <- runTask playAttrs "command" [json|{"command":"echo Starting scheduler -d /tmp/nodepool"}|] ([("_tmp_nodepool", file1)] <> baseEnv)
  debug0 <- runTask playAttrs "debug" [json|{"debug":null}|] ([] <> baseEnv)
  pure $ [file1] <> [command2] <> [debug0]

playLocalhost1 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost1 playAttrs baseEnv = do
  debug1 <- runTask playAttrs "debug" [json|{"debug":{"msg":"etc stats is {{ etc }}"}}|] ([] <> baseEnv)
  pure $ [debug1]

