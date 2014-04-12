{-# LANGUAGE OverloadedStrings #-}
module Main where

import Development.Scion.Core
import Development.Scion.Dispatcher

import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
   dispHdl <- newDispatcherHandle dispCfg
   defaultMain (tests dispHdl)
 where
   dispCfg = DispatcherConfig
     { dcGhcExecutable = "ghc" }

tests :: DispatcherHandle -> TestTree
tests dispHdl =
  testGroup "basic"
    [ testGroup "single-file"
      [ testCase "no-errors" $ do
          testCompile' "single-file/0000-no-errors.hs" $ \ok msgs -> do
            ok @?= True
            length msgs @?= 0
      , testCase "undefined-var" $ do
          testCompile' "single-file/0001-undefined-var.hs" $ \ok msgs -> do
            ok @?= False
            msgs @?= [Message Error (SourceSpan 0 13 0 15) (NotInScope "xyz")]
      ]
    ]
 where
   testCompile' = testCompile dispHdl

testCompile :: DispatcherHandle
            -> FilePath
            -> (Bool -> [Message] -> IO ()) -> IO ()
testCompile dispHdl file check = do
  rslt <- compileFile dispHdl ("tests/data" </> file)  ["-c", "-fforce-recomp"]
  case rslt of
    Left err -> fail $ show err
    Right rslt -> check (crSuccess rslt) (crMessages rslt)
