{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Development.Scion.Core where

import Data.Binary ( Binary(..) )
import Data.Monoid
import GHC.Generics

data SourceSpan = SourceSpan !Int !Int !Int !Int
  deriving (Eq, Ord, Show, Generic)

instance Binary SourceSpan

instance Monoid SourceSpan where
  mempty = SourceSpan 0 0 (-1) (-1)
  mappend (SourceSpan l1 c1 l2 c2) (SourceSpan l3 c3 l4 c4) =
    SourceSpan lmin cmin lmax cmax
   where (lmin, cmin) = min (l1, c1) (l3, c3)
         (lmax, cmax) = max (l2, c2) (l4, c4)
