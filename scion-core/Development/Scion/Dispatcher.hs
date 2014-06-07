{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Development.Scion.Dispatcher where

import Prelude hiding ( lines )

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.IORef
import Data.Monoid
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
  , dcGhcWorker     :: !FilePath
  }

-- | Handle to the state of the dispatcher
data DispatcherHandle = DispatcherHandle
  { dhConfig       :: !DispatcherConfig
  , dhLogger       :: !LoggerHandle
  , dhImportParser :: !(IORef (Maybe WorkerHandle))
  }

------------------------------------------------------------------------------

data DispatcherError = DispatcherError String
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------

newDispatcherHandle :: DispatcherConfig -> LoggerHandle -> IO DispatcherHandle
newDispatcherHandle cfg logger =
  DispatcherHandle cfg logger <$> newIORef Nothing

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
            -> IO (Either DispatcherError WorkerHandle)
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
      return $! Left $! DispatcherError msg
    Right (WorkerVersion n) -> do
      if n == [0] then
        return $ Right $! WorkerHandle inp out err pHdl outputReader
       else
        return $ Left $ DispatcherError $ "Unexpected worker version: " ++ show n
    Right ans ->
      return $ Left $ DispatcherError $ "Unexpected worker response: " ++ show ans

workerIpc :: DispatcherHandle -> WorkerHandle -> WorkerCommand
          -> IO (Either DispatcherError WorkerResponse)
workerIpc _dispHdl workerHdl cmd = do
  sendMessage (whStdin workerHdl) cmd
  messageOrErr <- recvMessage (whStdout workerHdl)
  case messageOrErr of
    Left msg -> do
      return (Left (DispatcherError msg))
    Right ans ->
      return (Right ans)
    -- Right ans ->
    --   return $ Left $ WorkerError $ "Unexpected worker response: " ++ show ans

------------------------------------------------------------------------------

getImportParser :: DispatcherHandle -> IO (Either DispatcherError WorkerHandle)
getImportParser dispHdl = do
  mbHdl <- readIORef (dhImportParser dispHdl)
  case mbHdl of
    Just wh -> return (Right wh)
    Nothing -> do
      errOrWorker <- startWorker dispHdl (dcGhcWorker (dhConfig dispHdl))
      case errOrWorker of
        Left err -> return $ Left $ DispatcherError $
                      "Could not start worker: " <> show err
        Right wh -> do
          ans <- workerIpc dispHdl wh (InitGhcWorker [])
          case ans of
            Left err -> return $ Left $ DispatcherError $
                          "Could not start worker: " <> show err
            Right (GhcWorkerReady _warns) -> do
              writeIORef (dhImportParser dispHdl) $! Just wh
              return (Right wh)
            _ ->
              return $ Left $ DispatcherError "Unexpected worker response"

------------------------------------------------------------------------------

getImports :: DispatcherHandle -> FilePath
           -> IO (Either DispatcherError ModuleHeader)
getImports dispHdl file = do
  optWorker <- getImportParser dispHdl
  case optWorker of
    Left err -> return (Left err)
    Right h -> do
      ans <- workerIpc dispHdl h $! ParseImports file
      case ans of
        Right (ParsedImports hdr) ->
          return $! Right hdr
        Left err ->
          return $! Left err
        _ ->
          return $ Left $ DispatcherError "Unexpected worker response"
