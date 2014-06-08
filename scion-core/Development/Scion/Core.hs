{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}
module Development.Scion.Core where

import Development.Scion.Binary
import Development.Scion.Types

import Control.Exception ( Exception(..), SomeException )
import Data.Binary ( Binary(..) )
import Data.Typeable ( Typeable )
import qualified Data.Text as T

import GHC.Generics


------------------------------------------------------------------------------

data CompilationResult = CompilationResult
  { crSuccess             :: !Bool
  , crFile                :: !FilePath
  , crMessages            :: [Message]
  } deriving (Eq, Ord, Show, Generic)

instance Binary CompilationResult where put = genput; get = genget

------------------------------------------------------------------------------

data CabalError = CabalError String
  deriving (Eq, Ord, Show, Typeable)

instance Exception CabalError
