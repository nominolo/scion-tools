{-# LANGUAGE ScopedTypeVariables #-}
module Development.Scion.Cabal where

import           Development.Scion.Core

import           Control.Exception
import           Development.Shake
import qualified Distribution.PackageDescription.Parse as PD
import qualified Distribution.PackageDescription as PD
import           Distribution.Simple.Build ( initialBuildSteps )
import           Distribution.Simple.Configure
import qualified Distribution.Simple.LocalBuildInfo as Lbi
import           Distribution.Simple.Program
import           Distribution.Simple.Program.GHC ( GhcOptions(..),
                   renderGhcOptions, GhcMode(..), GhcOptimisation(..) )
import           Distribution.Simple.Setup ( defaultConfigFlags,
                     ConfigFlags(..), Flag(..) )
import qualified Distribution.Verbosity as V
import           System.Exit
import           System.FilePath
import           System.FilePath.Canonical
------------------------------------------------------------------------------

data CabalConfig = CabalConfig
  { ccDistDir   :: !CanonicalFilePath
  , ccCabalFile :: !CanonicalFilePath
  , ccRootDir   :: !CanonicalFilePath
  } deriving (Eq, Ord, Show)

------------------------------------------------------------------------------

configureCabalProject :: FilePath
                      -> FilePath
                      -> Action ()
configureCabalProject cabalFile distDir = do
  need [cabalFile]

  genPkgDescr <- liftIO $ PD.readPackageDescription V.silent cabalFile

  let progConf = defaultProgramConfiguration

  let configFlags =
        (defaultConfigFlags progConf)
          { configDistPref    = Flag distDir
          , configVerbosity   = Flag V.normal
          , configUserInstall = Flag True
          -- TODO: parse flags properly
          }

  ioCatchExits $ do
    lbi <- configure (genPkgDescr, (Nothing, [])) configFlags
    writePersistBuildConfig distDir lbi
    initialBuildSteps distDir (Lbi.localPkgDescr lbi) lbi V.normal

  return ()

 where
   ioCatchExits act = liftIO $
     act `catches`
      [ Handler $ \(e :: ExitCode) -> do
          let msg = "Failed to configure: " ++ show e
          --addFileMetadata monit cabalFile CabalMeta [] [cabalErrorMsg msg]
          throwIO $ CabalError msg
      , Handler $ \(e :: IOException) -> do
          let msg = "Failed to configure: " ++ show e
          --addFileMetadata monit cabalFile CabalMeta [] [cabalErrorMsg msg]
          throwIO $ CabalError msg
      ]

------------------------------------------------------------------------------

dist :: CabalConfig -> FilePath -> FilePath
dist c path = canonicalFilePath (ccDistDir c) ++ path

distC :: CabalConfig -> String -> FilePath -> FilePath
distC c subdir path = (canonicalFilePath (ccDistDir c) </> subdir) ++ path

------------------------------------------------------------------------------

cabalRules :: CabalConfig -> Rules ()
cabalRules c = do

  dist c "/setup-config" *> \_out -> do
    configureCabalProject (canonicalFilePath $ ccCabalFile c) (dist c "")

  
