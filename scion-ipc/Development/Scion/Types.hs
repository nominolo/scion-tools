{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification,
    StandaloneDeriving, DeriveGeneric, OverloadedStrings
 #-}
module Development.Scion.Types where

import Development.Scion.Binary
import Data.Binary

import Control.Applicative
import Data.Int ( Int64 )
import GHC.Generics ( Generic )
import System.IO
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BLC8

------------------------------------------------------------------------------

data WorkerCommand
  = GetWorkerVersion
  | InitGhcWorker [T.Text]  -- GHC flags
  | ParseImports !FilePath
  deriving (Eq, Show, Generic)

instance Binary WorkerCommand where put = genput; get = genget

------------------------------------------------------------------------------

data WorkerResponse
  = WorkerVersion ![Int]
  | GhcWorkerReady [T.Text] -- warnings
  | WorkerFailure !T.Text
  deriving (Eq, Show, Generic)

instance Binary WorkerResponse where put = genput; get = genget
