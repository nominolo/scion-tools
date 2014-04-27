{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Development.Scion.Dispatcher where

--import Development.Scion.Core

import Prelude hiding ( lines )

import Control.Applicative
import Control.Concurrent.MVar
--import Control.Concurrent.Async
--import Control.Concurrent.STM
--import Control.Exception ( handle, SomeException )
--import Control.Monad
--import Data.Attoparsec.Char8 as Atto
--import System.IO ( Handle )
--import System.Exit
--import System.FilePath
--import System.Process
import qualified Data.ByteString.Char8 as BC8
-- import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T

------------------------------------------------------------------------------

data DispatcherConfig = DispatcherConfig
  { dcGhcExecutable :: !FilePath
  , dcScionCabal    :: !FilePath
  }

-- | Handle to the state of the dispatcher
data DispatcherHandle = DispatcherHandle
  { dhConfig        :: !DispatcherConfig
  , dhStdoutLock    :: !(MVar ())
  }

------------------------------------------------------------------------------

data DispatcherError = DispatcherError String
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------

newDispatcherHandle :: DispatcherConfig -> IO DispatcherHandle
newDispatcherHandle cfg = DispatcherHandle cfg <$> newMVar ()

safeLogLn :: DispatcherHandle -> BC8.ByteString -> IO ()
safeLogLn dh line =
  withMVar (dhStdoutLock dh) $ \_ -> BC8.putStrLn line
