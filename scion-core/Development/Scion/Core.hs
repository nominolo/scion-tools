{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}
module Development.Scion.Core where

import Development.Scion.Binary

import Control.Exception ( Exception(..), SomeException )
import Data.Binary ( Binary(..) )
import Data.Monoid
import Data.Typeable ( Typeable )
import qualified Data.Text as T

import GHC.Generics

------------------------------------------------------------------------------

-- | A range in the source program.  Both line and column indexes are
-- zero-based, i.e., @(0, 0)@ is the first position in every text file.
data SourceSpan = SourceSpan !Int !Int !Int !Int
  deriving (Eq, Ord, Show, Generic)

instance Binary SourceSpan where put = genput; get = genget

instance Monoid SourceSpan where
  mempty = SourceSpan 0 0 (-1) (-1)
  mappend (SourceSpan l1 c1 l2 c2) (SourceSpan l3 c3 l4 c4) =
    SourceSpan lmin cmin lmax cmax
   where (lmin, cmin) = min (l1, c1) (l3, c3)
         (lmax, cmax) = max (l2, c2) (l4, c4)

-- instance ToJSON SourceSpan where
--   toJSON (SourceSpan l1 c1 l2 c2) =
--     object ["span" .= toJSON [l1, c1, l2, c2]]

------------------------------------------------------------------------------

data Severity = Warning | Error
  deriving (Eq, Ord, Show, Generic)

-- | An error message produced by a tool.
data Message = Message !Severity !SourceSpan !MessageInfo
  deriving (Eq, Ord, Show, Generic)

data MessageInfo
  = OtherMessage !T.Text
  | NotInScope !T.Text
  deriving (Eq, Ord, Show, Generic)

instance Binary Severity    where put = genput; get = genget
instance Binary MessageInfo where put = genput; get = genget
instance Binary Message     where put = genput; get = genget

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
