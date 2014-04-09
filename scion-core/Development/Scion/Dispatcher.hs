module Development.Scion.Dispatcher where

import Development.Scion.Core

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import System.IO ( Handle )
import System.Exit
--import System.FilePath
import System.Process
import qualified Data.ByteString.Char8 as BC8

------------------------------------------------------------------------------

data DispatcherConfig = DispatcherConfig
  { dcGhcExecutable :: !FilePath }

-- | Handle to the state of the dispatcher
data DispatcherHandle = DispatcherHandle
  { dhConfig :: !DispatcherConfig }

------------------------------------------------------------------------------

data DispatcherError = DispatcherError String
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------

newDispatcherHandle :: DispatcherConfig -> IO DispatcherHandle
newDispatcherHandle = return . DispatcherHandle

------------------------------------------------------------------------------

-- TODO: Maybe args should be location annotated, so we can report where a
-- particular flag came from.  E.g., from the .cabal file.

compileFile :: DispatcherHandle   -- ^ Dispatcher handle
            -> FilePath -- ^ Source file to compile
            -> [String] -- ^ Compiler flags
            -> IO (Either DispatcherError CompilationResult)
compileFile hdl sourceFile flags = do
  let cfg = dhConfig hdl
  allOutput <- newCapturedOutput

  (_inp, out, err, pHdl)
      <- runInteractiveProcess (dcGhcExecutable cfg)
                               (flags ++ ["-ferror-spans", sourceFile])
                               Nothing -- no custom working dir
                               Nothing -- inherit environment

  outRdr <- async (captureOutput (coStdout allOutput) out)
  errRdr <- async (captureOutput (coStderr allOutput) err)

  exitCode <- waitForProcess pHdl

  -- TODO: Parse error messages from stderr; Line looks like:
  -- <sourceFile>:<location>:( Warning:)?\n, then message follows indented by 4
  -- spaces

  -- TODO: Could the threads just finish on their own?
  threadDelay 1000000

  cancel outRdr
  cancel errRdr

  let rslt = CompilationResult{ crSuccess = exitCode == ExitSuccess
                              , crFile = sourceFile
                              , crMessages = []
                              }
  return $ Right rslt

data CapturedOutput = CapturedOutput
  { coStdout :: !(TVar [BC8.ByteString])
  , coStderr :: !(TVar [BC8.ByteString])
  }

newCapturedOutput :: IO CapturedOutput
newCapturedOutput =
  CapturedOutput <$> newTVarIO []
                 <*> newTVarIO []

captureOutput :: TVar [BC8.ByteString] -> Handle -> IO ()
captureOutput tvar h =
  forever $ do
    line <- BC8.hGetLine h
    --print line
    atomically $ modifyTVar' tvar (line:)
