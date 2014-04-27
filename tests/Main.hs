{-# LANGUAGE OverloadedStrings #-}
module Main where

import Development.Scion.Cabal
import Development.Scion.Core
import Development.Scion.Dispatcher
import Development.Scion.Compile

import Control.Applicative
import Control.Monad
import Development.Shake as Shake
import System.Directory as Dir
import System.FilePath
import System.FilePath.Canonical
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
   dispHdl <- newDispatcherHandle dispCfg
   defaultMain (tests dispHdl)
 where
   dispCfg = DispatcherConfig
     { dcGhcExecutable = "ghc"
     , dcScionCabal = "scion-cabal/dist/build/scion-cabal/scion-cabal"
     }

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
      , testCase "undefined-var-primed1" $ do
          testCompile' "single-file/0002-undefined-var-primed1.hs" $ \ok msgs -> do
            ok @?= False
            msgs @?= [Message Error (SourceSpan 1 8 1 14) (NotInScope "zoo'bar")]
      , testCase "undefined-var-primed2" $ do
          testCompile' "single-file/0003-undefined-var-primed2.hs" $ \ok msgs -> do
            ok @?= False
            msgs @?= [Message Error (SourceSpan 1 8 1 15) (NotInScope "zoo'bar'")]
      , testCase "undefined-type" $ do
          testCompile' "single-file/0004-undefined-type.hs" $ \ok msgs -> do
            ok @?= False
            msgs @?= [Message Error (SourceSpan 0 19 0 21) (NotInScope "Foo")]
      ]
    , testGroup "shake"
      [ testCase "configure" $ do
          cleanProject "projects/hello"
          testShake dispHdl "projects/hello" "hello.cabal"
          assertFileExists "projects/hello/.scion/setup-config"
      -- TODO: Add tests for various Cabal failures (parse error, dependency not
      -- found, ...)
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

------------------------------------------------------------------------------

assertFileExists :: FilePath -> IO ()
assertFileExists file = do
  ok <- Dir.doesFileExist ("tests/data" </> file)
  assertBool ("File exists: " ++ file) ok 

cleanProject :: FilePath -> IO ()
cleanProject projectDir = do
  let dir = "tests/data" </> projectDir </> ".scion"
  ex <- Dir.doesDirectoryExist dir
  when ex $ removeDirectoryRecursive dir

------------------------------------------------------------------------------

testShake :: DispatcherHandle -> FilePath -> FilePath -> IO ()
testShake dispHdl projectDir0 cabalFile0 = do
  let projectDir = "tests" </> "data" </> projectDir0
  workDir <- canonical projectDir
  conf <- CabalConfig <$> canonical (projectDir </> ".scion")
                      <*> canonical (projectDir </> cabalFile0)
                      <*> pure workDir
  shake shakeOptions $ do
    cabalRules conf dispHdl
    want [dist conf "/setup-config"]
