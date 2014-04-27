{-# LANGUAGE ScopedTypeVariables #-}
module Development.Scion.Utils.IO where

import Development.Scion.Dispatcher

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception ( handle, SomeException )
import qualified Data.ByteString.Char8 as BC8
import System.IO ( Handle )

------------------------------------------------------------------------------

-- TODO: We can probably use "pipes" for this somehow.

data CapturedOutput = CapturedOutput
  { coStdout :: !LinesQueue
  , coStderr :: !LinesQueue
  }

type LinesQueue = TQueue (Maybe BC8.ByteString)

newCapturedOutput :: IO CapturedOutput
newCapturedOutput =
  CapturedOutput <$> newTQueueIO <*> newTQueueIO

captureProcessOutput :: Handle -- ^ Process's @stdout@
                     -> Handle -- ^ Process's @stderr@
                     -> (LinesQueue -> IO a)
                     -> (LinesQueue -> IO b)
                     -> IO (Async a, Async b)
captureProcessOutput hOut hErr processOut processErr = do
  qOut <- newTQueueIO
  qErr <- newTQueueIO
  outRdr <- async (captureOutput hOut qOut)
  errRdr <- async (captureOutput hErr qErr)
  (,) <$> async (processOut qOut <* wait outRdr)
      <*> async (processErr qErr <* wait errRdr)

captureOutput :: Handle -> LinesQueue -> IO ()
captureOutput h queue = loop
 where
   loop = do
     mbLine <- handle (\ (_e :: SomeException) -> return Nothing) $ do
                    Just <$> BC8.hGetLine h
     atomically $ writeTQueue queue mbLine
     case mbLine of
       Nothing    -> return ()
       Just _line -> loop

discardOutput :: LinesQueue -> IO ()
discardOutput q = loop
 where
   loop = do
     mbLine <- atomically $ readTQueue q
     case mbLine of
       Nothing -> return ()
       Just _l -> loop

printOutput :: DispatcherHandle -> LinesQueue -> IO ()
printOutput dh q = loop
 where
   loop = do
     mbLine <- atomically $ readTQueue q
     case mbLine of
       Nothing -> return ()
       Just l  -> safeLogLn dh l >> loop
