{-# LANGUAGE CPP, BangPatterns, TypeOperators, KindSignatures,
             TypeSynonymInstances, FlexibleInstances, FlexibleContexts,
             ScopedTypeVariables
  #-}
-- | Support for GHC Generics (since 7.6) with older binary packages.
--
-- This module needs to be used both by programs that link against the GHC API
-- as well as other modules.  When linking against the GHC API the program has
-- to use the same versions of the packages it depends on.  This includes in
-- particular an older version of the "binary" package which doesn't have
-- support for Generics.
--
-- Using the utilities in this package, we can declare an instance with very
-- little boilerplate code:
--
-- > data MyType = ... deriving Generics
--
-- > instance Binary MyType where put = genput; get = genget
--
module Development.Scion.Binary where

import Control.Applicative
import Data.Binary
import Data.Bits
import GHC.Generics

genput :: (Generic t, GBinary2 (Rep t)) => t -> Put
genput = gput2 . from

genget :: (Generic t, GBinary2 (Rep t)) => Get t
genget = to `fmap` gget2


class GBinary2 f where
  gput2 :: f t -> Put
  gget2 :: Get (f t)


-- Type without constructors
instance GBinary2 V1 where
    gput2 _ = return ()
    gget2   = return undefined

-- Constructor without arguments
instance GBinary2 U1 where
    gput2 U1 = return ()
    gget2    = return U1

-- Product: constructor with parameters
instance (GBinary2 a, GBinary2 b) => GBinary2 (a :*: b) where
    gput2 (x :*: y) = gput2 x >> gput2 y
    gget2 = (:*:) <$> gget2 <*> gget2

-- Metadata (constructor name, etc)
instance GBinary2 a => GBinary2 (M1 i c a) where
    gput2 = gput2 . unM1
    gget2 = M1 <$> gget2

-- Constants, additional parameters, and rank-1 recursion
instance Binary a => GBinary2 (K1 i a) where
    gput2 = put . unK1
    gget2 = K1 <$> get

-- Borrowed from the cereal package.

-- The following GBinary instance for sums has support for serializing
-- types with up to 2^64-1 constructors. It will use the minimal
-- number of bytes needed to encode the constructor. For example when
-- a type has 2^8 constructors or less it will use a single byte to
-- encode the constructor. If it has 2^16 constructors or less it will
-- use two bytes, and so on till 2^64-1.

#define GUARD(WORD) (size - 1) <= fromIntegral (maxBound :: WORD)
#define PUTSUM(WORD) GUARD(WORD) = putSum (0 :: WORD) (fromIntegral size)
#define GETSUM(WORD) GUARD(WORD) = (get :: Get WORD) >>= checkGetSum (fromIntegral size)

instance ( GSum     a, GSum     b
         , GBinary2 a, GBinary2 b
         , SumSize    a, SumSize    b) => GBinary2 (a :+: b) where
    gput2 | PUTSUM(Word8) | PUTSUM(Word16) | PUTSUM(Word32) | PUTSUM(Word64)
         | otherwise = sizeError "encode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)
    {-# INLINE gput2 #-}

    gget2 | GETSUM(Word8) | GETSUM(Word16) | GETSUM(Word32) | GETSUM(Word64)
         | otherwise = sizeError "decode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)
    {-# INLINE gget2 #-}

sizeError :: Show size => String -> size -> error
sizeError s size =
    error $ "Can't " ++ s ++ " a type with " ++ show size ++ " constructors"

------------------------------------------------------------------------

checkGetSum :: (Ord word, Num word, Bits word, GSum f)
            => word -> word -> Get (f a)
checkGetSum size code | code < size = getSum code size
                      | otherwise   = fail "Unknown encoding for constructor"
{-# INLINE checkGetSum #-}

class GSum f where
    getSum :: (Ord word, Num word, Bits word) => word -> word -> Get (f a)
    putSum :: (Num w, Bits w, Binary w) => w -> w -> f a -> Put

instance (GSum a, GSum b, GBinary2 a, GBinary2 b) => GSum (a :+: b) where
    getSum !code !size | code < sizeL = L1 <$> getSum code           sizeL
                       | otherwise    = R1 <$> getSum (code - sizeL) sizeR
        where
          sizeL = size `shiftR` 1
          sizeR = size - sizeL
    {-# INLINE getSum #-}

    putSum !code !size s = case s of
                             L1 x -> putSum code           sizeL x
                             R1 x -> putSum (code + sizeL) sizeR x
        where
          sizeL = size `shiftR` 1
          sizeR = size - sizeL
    {-# INLINE putSum #-}

instance GBinary2 a => GSum (C1 c a) where
    getSum _ _ = gget2
    {-# INLINE getSum #-}

    putSum !code _ x = put code *> gput2 x
    {-# INLINE putSum #-}

------------------------------------------------------------------------

class SumSize f where
    sumSize :: Tagged f Word64

newtype Tagged (s :: * -> *) b = Tagged {unTagged :: b}

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
    sumSize = Tagged $ unTagged (sumSize :: Tagged a Word64) +
                       unTagged (sumSize :: Tagged b Word64)

instance SumSize (C1 c a) where
    sumSize = Tagged 1
