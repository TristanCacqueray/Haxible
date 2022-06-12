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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/dependency.yaml" expect (playbook [] [])
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
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|]), ("vars", [json|{"info_dir":"/tmp/info"}|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  debugInstallPackages0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Installing podman"}|]), ("name", [json|"Install packages..."|])]) localVars
  debugStartService0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Running podman run -it --rm quay.io/software-factory/ci-log-processor"}|]), ("vars", [json|{"requires":"_provider"}|]), ("name", [json|"Start service"|])]) ([("_provider", debugInstallPackages0)] <> localVars)
  fileCreateInfoDirectory0 <- runTask src playAttrs defaultVars "file" ([("file", [json|{"path":"{{ info_dir }}","state":"directory"}|]), ("name", [json|"Create info directory"|])]) localVars
  copyCopyInfoLog0 <- runTask src playAttrs defaultVars "copy" ([("copy", [json|{"content":"Log","dest":"{{ info_dir }}/log"}|]), ("name", [json|"Copy info log"|])]) ([("_fake_InfoDir", fileCreateInfoDirectory0)] <> localVars)
  pure $ [debugInstallPackages0] <> [debugStartService0] <> [fileCreateInfoDirectory0] <> [copyCopyInfoLog0]

