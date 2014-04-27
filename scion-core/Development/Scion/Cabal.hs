{-# LANGUAGE ScopedTypeVariables #-}
module Development.Scion.Cabal where

import           Development.Scion.Dispatcher
import           Development.Scion.Utils.IO

import           Control.Monad
import           Development.Shake
import           Control.Concurrent.Async
import           System.FilePath
import           System.Exit
import           System.FilePath.Canonical
import           System.Process
------------------------------------------------------------------------------

data CabalConfig = CabalConfig
  { ccDistDir   :: !CanonicalFilePath
  , ccCabalFile :: !CanonicalFilePath
  , ccRootDir   :: !CanonicalFilePath
  } deriving (Eq, Ord, Show)

------------------------------------------------------------------------------

-- TODO: This should parse/collect Cabal error messages
configureCabalProject :: DispatcherHandle
                      -> FilePath
                      -> FilePath
                      -> Action Bool
configureCabalProject hdl cabalFile distDir = do
  need [cabalFile]

  liftIO $ do
    (_inp, out, err, pHdl)
       <- runInteractiveProcess
            (dcScionCabal (dhConfig hdl))
            [ "--distDir", distDir, "configure"
            , cabalFile
            ]
            (Just (dropFileName cabalFile))
            Nothing

    (aOut, aErr) <- captureProcessOutput out err
                      (printOutput hdl) (printOutput hdl)

    exitCode <- waitForProcess pHdl

    _ <- wait aOut
    _ <- wait aErr

    return $! exitCode == ExitSuccess

------------------------------------------------------------------------------

dist :: CabalConfig -> FilePath -> FilePath
dist c path = canonicalFilePath (ccDistDir c) ++ path

distC :: CabalConfig -> String -> FilePath -> FilePath
distC c subdir path = (canonicalFilePath (ccDistDir c) </> subdir) ++ path

------------------------------------------------------------------------------

cabalRules :: CabalConfig -> DispatcherHandle -> Rules ()
cabalRules c hdl = do

  dist c "/setup-config" *> \_out -> do
    ok <- configureCabalProject hdl (canonicalFilePath $ ccCabalFile c) (dist c "")
    when (not ok) $
      fail "Could not configure"

  
