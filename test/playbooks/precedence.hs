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
  resultsLocalhost0 <- playLocalhost0 playAttrs  localVars
  resultsLocalhost1 <- playLocalhost1 playAttrs  localVars
  resultsLocalhost2 <- playLocalhost2 playAttrs  localVars
  resultsLocalhost3 <- playLocalhost3 playAttrs  localVars
  resultsLocalhost4 <- playLocalhost4 playAttrs  localVars
  resultsLocalhost5 <- playLocalhost5 playAttrs  localVars
  pure $ resultsLocalhost0 <> resultsLocalhost1 <> resultsLocalhost2 <> resultsLocalhost3 <> resultsLocalhost4 <> resultsLocalhost5

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsPrint0 <- rolePrint0 playAttrs  localVars
  pure $ resultsPrint0

playLocalhost1 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost1 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|]), ("vars", [json|{"print_arg":"priority"}|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsPrint1 <- rolePrint1 playAttrs  localVars
  pure $ resultsPrint1

playLocalhost2 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost2 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|]), ("vars", [json|{"print_arg":"shadow"}|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsPrint2 <- rolePrint2 playAttrs  ([("print_arg", [json|"priority"|])] <> localVars)
  pure $ resultsPrint2

playLocalhost3 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost3 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|]), ("vars", [json|{"print_arg":"shadow"}|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  block0 <- block0 playAttrs  ([("print_arg", [json|"priority"|])] <> localVars)
  pure $ block0

playLocalhost4 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost4 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|]), ("vars", [json|{"print_arg":"shadow"}|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  block2 <- block2 playAttrs  ([("print_arg", [json|"shadow"|])] <> localVars)
  pure $ block2

playLocalhost5 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost5 playAttrs' localVars = do
  let playAttrs = [("gather_facts", [json|false|]), ("hosts", [json|"localhost"|]), ("vars", [json|{"print_arg":"shadow"}|])] <> playAttrs'
      defaultVars = []
      src = "test/playbooks"
  block4 <- block4 playAttrs  ([("print_arg", [json|"shadow"|])] <> localVars)
  pure $ block4

block4 :: Vars -> Vars -> AnsibleHaxl [Value]
block4 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks"
  block5 <- block5 playAttrs  ([("print_arg", [json|"shadow"|])] <> localVars)
  pure $ block5

block5 :: Vars -> Vars -> AnsibleHaxl [Value]
block5 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsPrint5 <- rolePrint5 playAttrs  ([("print_arg", [json|"priority"|])] <> localVars)
  pure $ resultsPrint5

rolePrint5 :: Vars -> Vars -> AnsibleHaxl [Value]
rolePrint5 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = [("print_arg", [json|"priority (from role default)"|])]
      src = "test/playbooks/roles/print"
  debug5 <- runTask src playAttrs defaultVars "debug" ([("debug", [json|{"msg":"Print role {{ print_arg }}"}|])]) localVars
  pure $ [debug5]

block2 :: Vars -> Vars -> AnsibleHaxl [Value]
block2 playAttrs' localVars = do
  let playAttrs = playAttrs'
      defaultVars = []
      src = "test/playbooks"
  resultsPrint4 <- rolePrint4 playAttrs  ([("print_arg", [json|"priority"|])] <> localVars)
  pure $ resultsPrint4

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
  resultsPrint3 <- rolePrint3 playAttrs  localVars
  pure $ resultsPrint3

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

