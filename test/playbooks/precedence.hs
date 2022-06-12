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
main = Haxible.Eval.runHaxible "inventory.yaml" "test/playbooks/precedence.yaml" expect (playbook [] [])
  where expect = [[json|{"changed":false,"msg":"Print role priority (from role default)"}|], [json|{"changed":false,"msg":"Print role priority"}|], [json|{"changed":false,"msg":"Print role priority"}|], [json|{"changed":false,"msg":"Print role priority"}|], [json|{"changed":false,"msg":"Print role priority"}|], [json|{"changed":false,"msg":"Print role priority"}|]]

playbook :: Vars -> Vars -> AnsibleHaxl [Value]
playbook playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = ""
  resultsPlayLocalhost0 <- playLocalhost0 playAttrs (localVars <> defaultVars)
  resultsPlayLocalhost1 <- playLocalhost1 playAttrs (localVars <> defaultVars)
  resultsPlayLocalhost2 <- playLocalhost2 playAttrs (localVars <> defaultVars)
  resultsPlayLocalhost3 <- playLocalhost3 playAttrs (localVars <> defaultVars)
  resultsPlayLocalhost4 <- playLocalhost4 playAttrs (localVars <> defaultVars)
  resultsPlayLocalhost5 <- playLocalhost5 playAttrs (localVars <> defaultVars)
  pure $ resultsPlayLocalhost0 <> resultsPlayLocalhost1 <> resultsPlayLocalhost2 <> resultsPlayLocalhost3 <> resultsPlayLocalhost4 <> resultsPlayLocalhost5

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsRolePrint0 <- rolePrint0 playAttrs (localVars <> defaultVars)
  pure $ resultsRolePrint0

playLocalhost1 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost1 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|]), ("vars", [json|{"print_arg":"priority"}|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsRolePrint1 <- rolePrint1 playAttrs (localVars <> defaultVars)
  pure $ resultsRolePrint1

playLocalhost2 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost2 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|]), ("vars", [json|{"print_arg":"shadow"}|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsRolePrint2 <- rolePrint2 playAttrs ([("print_arg", [json|"priority"|])] <> localVars <> defaultVars)
  pure $ resultsRolePrint2

playLocalhost3 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost3 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|]), ("vars", [json|{"print_arg":"shadow"}|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsBlock0 <- block0 playAttrs ([("print_arg", [json|"priority"|])] <> localVars <> defaultVars)
  pure $ resultsBlock0

playLocalhost4 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost4 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|]), ("vars", [json|{"print_arg":"shadow"}|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsBlock1 <- block1 playAttrs ([("print_arg", [json|"shadow"|])] <> localVars <> defaultVars)
  pure $ resultsBlock1

playLocalhost5 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost5 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|]), ("vars", [json|{"print_arg":"shadow"}|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsBlock2 <- block2 playAttrs ([("print_arg", [json|"shadow"|])] <> localVars <> defaultVars)
  pure $ resultsBlock2

block2 :: Vars -> Vars -> AnsibleHaxl [Value]
block2 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsBlock3 <- block3 playAttrs ([("print_arg", [json|"shadow"|])] <> localVars <> defaultVars)
  pure $ resultsBlock3

block3 :: Vars -> Vars -> AnsibleHaxl [Value]
block3 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsRolePrint5 <- rolePrint5 playAttrs ([("print_arg", [json|"priority"|])] <> localVars <> defaultVars)
  pure $ resultsRolePrint5

rolePrint5 :: Vars -> Vars -> AnsibleHaxl [Value]
rolePrint5 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = [("print_arg", [json|"priority (from role default)"|])]
      src = "test/playbooks/roles/print"
  debug5 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Print role {{ print_arg }}"}|])]) localVars
  pure $ [debug5]

block1 :: Vars -> Vars -> AnsibleHaxl [Value]
block1 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsRolePrint4 <- rolePrint4 playAttrs ([("print_arg", [json|"priority"|])] <> localVars <> defaultVars)
  pure $ resultsRolePrint4

rolePrint4 :: Vars -> Vars -> AnsibleHaxl [Value]
rolePrint4 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = [("print_arg", [json|"priority (from role default)"|])]
      src = "test/playbooks/roles/print"
  debug4 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Print role {{ print_arg }}"}|])]) localVars
  pure $ [debug4]

block0 :: Vars -> Vars -> AnsibleHaxl [Value]
block0 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsRolePrint3 <- rolePrint3 playAttrs (localVars <> defaultVars)
  pure $ resultsRolePrint3

rolePrint3 :: Vars -> Vars -> AnsibleHaxl [Value]
rolePrint3 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = [("print_arg", [json|"priority (from role default)"|])]
      src = "test/playbooks/roles/print"
  debug3 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Print role {{ print_arg }}"}|])]) localVars
  pure $ [debug3]

rolePrint2 :: Vars -> Vars -> AnsibleHaxl [Value]
rolePrint2 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = [("print_arg", [json|"priority (from role default)"|])]
      src = "test/playbooks/roles/print"
  debug2 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Print role {{ print_arg }}"}|])]) localVars
  pure $ [debug2]

rolePrint1 :: Vars -> Vars -> AnsibleHaxl [Value]
rolePrint1 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = [("print_arg", [json|"priority (from role default)"|])]
      src = "test/playbooks/roles/print"
  debug1 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Print role {{ print_arg }}"}|])]) localVars
  pure $ [debug1]

rolePrint0 :: Vars -> Vars -> AnsibleHaxl [Value]
rolePrint0 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = [("print_arg", [json|"priority (from role default)"|])]
      src = "test/playbooks/roles/print"
  debug0 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Print role {{ print_arg }}"}|])]) localVars
  pure $ [debug0]

