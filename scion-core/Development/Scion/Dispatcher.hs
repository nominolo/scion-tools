{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Development.Scion.Dispatcher where

import Prelude hiding ( lines )

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
--import qualified Data.ByteString.Char8 as BC8
import           System.IO
import           System.Process


import Development.Scion.Types
import Development.Scion.Message
import Development.Scion.Utils.IO

------------------------------------------------------------------------------

data DispatcherConfig = DispatcherConfig
  { dcGhcExecutable :: !FilePath
  , dcScionCabal    :: !FilePath
  }

-- | Handle to the state of the dispatcher
data DispatcherHandle = DispatcherHandle
  { dhConfig    :: !DispatcherConfig
  , dhLogger    :: !LoggerHandle
  }

------------------------------------------------------------------------------

data DispatcherError = DispatcherError String
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------

newDispatcherHandle :: DispatcherConfig -> LoggerHandle -> IO DispatcherHandle
newDispatcherHandle cfg logger =
  return $ DispatcherHandle cfg logger

------------------------------------------------------------------------------

data WorkerHandle = WorkerHandle
  { whStdin   :: !Handle
  , whStdout  :: !Handle
  , whStderr  :: !Handle
  , whProcess :: !ProcessHandle
  , whOutputReader :: !(Async ())
  }

data WorkerError = WorkerError String   -- for now
  deriving Show

startWorker :: DispatcherHandle -> FilePath
            -> IO (Either WorkerError WorkerHandle)
startWorker dh path = do
  (inp, out, err, pHdl)
    <- runInteractiveProcess path [] Nothing Nothing

  -- TODO: Abstract me!
  qErr <- newTQueueIO
  errRdr <- async $ captureOutput err qErr
  outputReader <- async $ printOutputPrefixed (dhLogger dh) "w: " qErr <* wait errRdr

  sendMessage inp GetWorkerVersion
  messageOrErr <- recvMessage out
  case messageOrErr of
    Left msg -> do
      return (Left (WorkerError msg))
    Right (WorkerVersion n) -> do
      if n == [0] then
        return $ Right $! WorkerHandle inp out err pHdl outputReader
       else
        return $ Left $ WorkerError $ "Unexpected worker version: " ++ show n
    Right ans ->
      return $ Left $ WorkerError $ "Unexpected worker response: " ++ show ans

workerIpc :: DispatcherHandle -> WorkerHandle -> WorkerCommand
          -> IO (Either WorkerError WorkerResponse)
workerIpc _dispHdl workerHdl cmd = do
  sendMessage (whStdin workerHdl) cmd
  messageOrErr <- recvMessage (whStdout workerHdl)
  case messageOrErr of
    Left msg -> do
      return (Left (WorkerError msg))
    Right ans ->
      return (Right ans)
    -- Right ans ->
    --   return $ Left $ WorkerError $ "Unexpected worker response: " ++ show ans
