{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, KindSignatures,
    ScopedTypeVariables, TypeOperators, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Generic
-- Copyright   : Bryan O'Sullivan
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Bryan O'Sullivan <bos@serpentine.com>
-- Stability   : unstable
-- Portability : Only works with GHC 7.2 and newer
--
-- Instances for supporting GHC generics.
--
-----------------------------------------------------------------------------
module Data.Binary.Generic where

import Control.Applicative
import Data.Binary
import Data.Bits
import GHC.Generics
import Prelude

class GGBinary f where
    ggput :: f t -> Put
    ggget :: Get (f t)

-- Type without constructors
instance GGBinary V1 where
    ggput _ = return ()
    ggget   = return undefined

-- Constructor without arguments
instance GGBinary U1 where
    ggput U1 = return ()
    ggget    = return U1

-- Product: constructor with parameters
instance (GGBinary a, GGBinary b) => GGBinary (a :*: b) where
    ggput (x :*: y) = ggput x >> ggput y
    ggget = (:*:) <$> ggget <*> ggget

-- Metadata (constructor name, etc)
instance GGBinary a => GGBinary (M1 i c a) where
    ggput = ggput . unM1
    ggget = M1 <$> ggget

-- Constants, additional parameters, and rank-1 recursion
instance Binary a => GGBinary (K1 i a) where
    ggput = put . unK1
    ggget = K1 <$> get

-- Borrowed from the cereal package.

-- The following GGBinary instance for sums has support for serializing
-- types with up to 2^64-1 constructors. It will use the minimal
-- number of bytes needed to encode the constructor. For example when
-- a type has 2^8 constructors or less it will use a single byte to
-- encode the constructor. If it has 2^16 constructors or less it will
-- use two bytes, and so on till 2^64-1.

#define GUARD(WORD) (size - 1) <= fromIntegral (maxBound :: WORD)
#define PUTSUM(WORD) GUARD(WORD) = putSum (0 :: WORD) (fromIntegral size)
#define GETSUM(WORD) GUARD(WORD) = (get :: Get WORD) >>= checkGetSum (fromIntegral size)

instance ( GSum     a, GSum     b
         , SumSize    a, SumSize    b) => GGBinary (a :+: b) where
    ggput | PUTSUM(Word8) | PUTSUM(Word16) | PUTSUM(Word32) | PUTSUM(Word64)
         | otherwise = sizeError "encode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)
    {-# INLINE ggput #-}

    ggget | GETSUM(Word8) | GETSUM(Word16) | GETSUM(Word32) | GETSUM(Word64)
         | otherwise = sizeError "decode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)
    {-# INLINE ggget #-}

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

instance (GSum a, GSum b) => GSum (a :+: b) where
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

instance GGBinary a => GSum (C1 c a) where
    getSum _ _ = ggget
    {-# INLINE getSum #-}

    putSum !code _ x = put code *> ggput x
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
