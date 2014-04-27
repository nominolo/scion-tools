{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Development.Scion.Dispatcher where

import Prelude hiding ( lines )

import Control.Applicative
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as BC8

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
