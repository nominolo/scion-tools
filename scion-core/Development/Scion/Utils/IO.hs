{-# LANGUAGE ScopedTypeVariables #-}
module Development.Scion.Utils.IO where

--import Development.Scion.Dispatcher

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception ( handle, SomeException )
import Control.Monad ( when )
import Data.Maybe ( isJust )
import Data.Monoid
import GHC.IO.Handle ( hDuplicateTo, hDuplicate )
import System.IO ( Handle, hFlush, hGetEncoding, hSetBinaryMode )
import qualified Data.ByteString.Char8 as BC8

------------------------------------------------------------------------------

data LoggerHandle = LoggerHandle
  { lhStdoutLock    :: !(MVar ())
  }

newLoggerHandle :: IO LoggerHandle
newLoggerHandle = LoggerHandle <$> newMVar ()

safeLogLn :: LoggerHandle -> BC8.ByteString -> IO ()
safeLogLn lh line =
  withMVar (lhStdoutLock lh) $ \_ -> BC8.putStrLn line

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

printOutput :: LoggerHandle -> LinesQueue -> IO ()
printOutput lh q = loop
 where
   loop = do
     mbLine <- atomically $ readTQueue q
     case mbLine of
       Nothing -> return ()
       Just l  -> safeLogLn lh l >> loop

printOutputPrefixed :: LoggerHandle -> BC8.ByteString -> LinesQueue -> IO ()
printOutputPrefixed lh prefix q = loop
 where
   loop = do
     mbLine <- atomically $ readTQueue q
     case mbLine of
       Nothing -> return ()
       Just l  -> safeLogLn lh (prefix <> l) >> loop

------------------------------------------------------------------------------

-- | Ensure that the handle is in binary mode.
ensureBinaryMode :: Handle -> IO ()
ensureBinaryMode h = do
  enc <- hGetEncoding h
  when (isJust enc) $
    hSetBinaryMode h True

-- | Get exclusive access to the first handle's resource.
--
-- Subsequent writes to the first handle are redirected to the second
-- handle.  The returned handle is an exclusive handle to the resource
-- initially held by the first handle.
makeExclusive ::
     Handle -- ^ The handle to the resource that we want exclusive
            -- access to.
  -> Handle -- ^ Anything written to the original handle will be
            -- redirected to this one.
  -> IO Handle -- ^ The exclusive handle.
makeExclusive hexcl hredirect = do
  hFlush hexcl
  hFlush hredirect
  hresult <- hDuplicate hexcl
  hDuplicateTo hredirect hexcl
  return hresult
