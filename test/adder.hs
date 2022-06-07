-- Generated with haxible
{-# LANGUAGE QuasiQuotes, ApplicativeDo, OverloadedStrings #-}
{- cabal:
build-depends: base, haxible
ghc-options: -threaded -rtsopts -with-rtsopts=-N -with-rtsopts=-T
-}
module Main (main) where

import Haxible.Eval

main :: IO ()
main = runHaxible "inventory.yaml" (playbook [] [])

playbook :: Vars -> Vars -> AnsibleHaxl [Value]
playbook playAttrs baseEnv = do
  resultsLocalhost0 <- playLocalhost0 ([("gather_facts", [json|false|]), ("hosts", [json|"localhost"|])] <> playAttrs) ([] <> [] <> baseEnv)
  resultsBackend0 <- playBackend0 ([("gather_facts", [json|false|]), ("hosts", [json|"backend"|])] <> playAttrs) ([("answer", resultsLocalhost0 !! 0)] <> [] <> baseEnv)
  pure $ resultsLocalhost0 <> resultsBackend0

playLocalhost0 :: Vars -> Vars -> AnsibleHaxl [Value]
playLocalhost0 playAttrs baseEnv = do
  debug0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"42"}}|] ([] <> baseEnv)
  assert0 <- runTask playAttrs "assert" [json|{"assert":{"that":["answer['msg'] == '42'"]}}|] ([("answer", debug0)] <> baseEnv)
  pure $ [debug0] <> [assert0]

playBackend0 :: Vars -> Vars -> AnsibleHaxl [Value]
playBackend0 playAttrs baseEnv = do
  roleAdder0 <- roleAdder ([] <> playAttrs) ([] <> [("x", [json|"{{ answer['msg'] }}"|]), ("y", [json|"21"|])] <> baseEnv)
  debug1 <- runTask playAttrs "debug" [json|{"debug":{"msg":"Over!"}}|] ([] <> baseEnv)
  pure $ roleAdder0 <> [debug1]

roleAdder :: Vars -> Vars -> AnsibleHaxl [Value]
roleAdder playAttrs baseEnv = do
  debugAddingNumbers0 <- runTask playAttrs "debug" [json|{"debug":{"msg":"Adding {{ x }} + {{ y }}"},"name":"Adding numbers"}|] ([] <> baseEnv)
  assertCheckingResults0 <- runTask playAttrs "assert" [json|{"assert":{"that":["x == '42' and y == '21'","add_result['msg'] == 'Adding 42 + 21'"]},"name":"Checking results"}|] ([("add_result", debugAddingNumbers0)] <> baseEnv)
  pure $ [debugAddingNumbers0] <> [assertCheckingResults0]

