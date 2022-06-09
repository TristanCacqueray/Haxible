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
main = runHaxible "inventory.yaml" "test/playbooks/dependency.yaml" (playbook [] [] [])

playbook :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playbook parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = parentPlayAttrs
  resultsLocalhost0 <- playLocalhost0 playAttrs (taskAttrs) (taskVars)
  pure $ resultsLocalhost0

playLocalhost0 :: Vars -> Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 parentPlayAttrs taskAttrs taskVars = do
  let playAttrs = [("hosts", [json|"localhost"|]), ("vars", [json|{"info_dir":"/tmp/info"}|])] <> parentPlayAttrs
  debugInstallPackages0 <- runTask playAttrs "debug" ([("debug", [json|{"msg":"Installing podman"}|]), ("name", [json|"Install packages..."|])] <> taskAttrs) (taskVars)
  debugStartService0 <- runTask playAttrs "debug" ([("debug", [json|{"msg":"Running podman run -it --rm quay.io/software-factory/ci-log-processor"}|]), ("name", [json|"Start service"|])] <> taskAttrs) ([("_provider", debugInstallPackages0)] <> taskVars)
  fileCreateInfoDirectory0 <- runTask playAttrs "file" ([("file", [json|{"path":"{{ info_dir }}","state":"directory"}|]), ("name", [json|"Create info directory"|])] <> taskAttrs) (taskVars)
  copyCopyInfoLog0 <- runTask playAttrs "copy" ([("copy", [json|{"content":"Log","dest":"{{ info_dir }}/log"}|]), ("name", [json|"Copy info log"|])] <> taskAttrs) ([("_fake_InfoDir", fileCreateInfoDirectory0)] <> taskVars)
  pure $ [debugInstallPackages0] <> [debugStartService0] <> [fileCreateInfoDirectory0] <> [copyCopyInfoLog0]

