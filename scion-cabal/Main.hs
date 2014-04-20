module Main where

import           Control.Applicative
import           Control.Exception
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
import           Options.Applicative
import           System.Directory
import           System.Exit
import           System.FilePath
--import           System.FilePath.Canonical

------------------------------------------------------------------------------

data Options = Options
  { oWorkingDir :: !FilePath
  , oDistDir    :: !FilePath
  , oCommand    :: !Command
  } deriving Show

data Command
  = Configure !ConfigureOptions
  deriving Show

data ConfigureOptions = ConfigureOptions
  { coCabalFile   :: !FilePath
  , coEnableTests :: !Bool
  } deriving Show

parseOptions :: Parser Command -> Parser Options
parseOptions parseCommand =
  Options
    <$> strOption (long "workingDir" <>
                   metavar "DIR" <>
                   value "." <>
                   help "Directory in which the command is executed\
                        \ (default: current working directory)")
    <*> strOption (long "distDir" <>
                   metavar "DIR" <>
                   value "dist" <>
                   help "Directory where output files are put (default:\
                        \ dist)")
    <*> parseCommand

parseConfigureOptions :: Parser ConfigureOptions
parseConfigureOptions =
  ConfigureOptions
    <$> argument Just (metavar "CABALFILE")
    <*> switch (long "enable-tests" <>
                help "Enable test suites")

parseCommand :: Parser Command
parseCommand = subparser $
  command "configure"
    (info (Configure <$> parseConfigureOptions)
          (progDesc "Configure"))

main :: IO ()
main = execParser opts >>= execCommand
 where
   opts = info (helper <*> parseOptions parseCommand)
            (fullDesc <>
             progDesc "Execute Cabal commands for Scion")

execCommand :: Options -> IO ()
execCommand opts = do
  setCurrentDirectory (oWorkingDir opts)
  let distDir = oDistDir opts

  case oCommand opts of
    Configure confOpts -> do

      genPkgDescr <- PD.readPackageDescription V.silent (coCabalFile confOpts)

      let progConf = defaultProgramConfiguration

      let configFlags =
            (defaultConfigFlags progConf)
              { configDistPref    = Flag distDir
              , configVerbosity   = Flag V.normal
              , configUserInstall = Flag True
              -- TODO: parse flags properly
              }

      --ioCatchExits $ do
      lbi <- configure (genPkgDescr, (Nothing, [])) configFlags
      writePersistBuildConfig distDir lbi
      initialBuildSteps distDir (Lbi.localPkgDescr lbi) lbi V.normal

      return ()
{-
 where
   ioCatchExits act =
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
-}    
