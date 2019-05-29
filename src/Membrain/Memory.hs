{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains 'Memory' data type.

module Membrain.Memory
       ( -- * Data type
         Memory (..)
       , memory
       , toMemory
       , showMemory

         -- * Conversion functions
       , toBits
       , toRat
       , floor

         -- * Numeric operations
       , memoryMul
       , memoryDiff
       , memoryPlus
       , memoryDiv
       ) where

import Prelude hiding (floor)

import Data.Coerce (coerce)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (..))
import Data.Ratio (Ratio, (%))
import Data.Semigroup (Semigroup (..))
import GHC.TypeNats (KnownNat, Nat, natVal)
import Numeric.Natural (Natural)

import Membrain.Units (KnownUnitSymbol, unitSymbol)

import qualified Prelude


{- | Main memory units type. It has phantom type parameter @mem@ of kind 'Nat'
which is type level representation of the unit.
-}
newtype Memory (mem :: Nat) = Memory
    { unMemory :: Natural
    } deriving (Show, Read, Eq, Ord)

instance Semigroup (Memory (mem :: Nat)) where
    (<>) :: Memory mem -> Memory mem -> Memory mem
    (<>) = coerce ((+) @Natural)
    {-# INLINE (<>) #-}

    sconcat :: NonEmpty (Memory mem) -> Memory mem
    sconcat = foldl' (<>) mempty
    {-# INLINE sconcat #-}

    stimes :: Integral b => b -> Memory mem -> Memory mem
    stimes n (Memory m) = Memory (fromIntegral n * m)
    {-# INLINE stimes #-}

instance Monoid (Memory (mem :: Nat)) where
    mempty :: Memory mem
    mempty = Memory 0
    {-# INLINE mempty #-}

    mappend :: Memory mem -> Memory mem -> Memory mem
    mappend = (<>)
    {-# INLINE mappend #-}

    mconcat :: [Memory mem] -> Memory mem
    mconcat = foldl' (<>) mempty
    {-# INLINE mconcat #-}

{- |
This 'showMemory' function shows 'Memory' value as 'Double' with measure unit
suffix. It shows 'Memory' losslessly while used with standardized units of
measures. The following math fact is used to display 'Memory'.

A decimal representation written with a repeating final @0@ is said to terminate
before these zeros. Instead of @1.585000...@ one simply writes @1.585@. The
decimal is also called a terminating decimal. Terminating decimals represent
rational numbers of the form \( \cfrac{k}{2^n 5^m} \). If you use units of the
different form then the 'show' function for 'Memory' hangs.
TODO: check this statically
-}
showMemory :: forall mem . (KnownNat mem, KnownUnitSymbol mem) => Memory mem -> String
showMemory (Memory m) = showFrac m (nat @mem) ++ unitSymbol @mem
  where
    showFrac :: Natural -> Natural -> String
    showFrac number d = goIntegral number
      where
        -- take integral part of fraction
        goIntegral :: Natural -> String
        goIntegral n =
            let (q, r) = n `divMod` d
                integral = show q
            in if r == 0
               then integral
               else integral ++ '.' : goFractional r

        -- convert reminder to fractional part
        goFractional :: Natural -> String
        goFractional 0 = ""
        goFractional n =
            let (q, r) = (n * 10) `divMod` d
            in show q ++ goFractional r

-- | Creates 'Memory' of unit 'mem' by given 'Natural' number.
-- 'Memory's smart constructor.
memory :: forall (mem :: Nat) . KnownNat mem => Natural -> Memory mem
memory = Memory . (* nat @mem)
{-# INLINE memory #-}

{- | Convert memory from one unit to another.

__Note:__ this changes only view, not model.
So this operation has zero runtime cost.
-}
toMemory :: forall (to :: Nat) (from :: Nat) . Memory from -> Memory to
toMemory = coerce
{-# INLINE toMemory #-}

{- | Lossless 'Memory' conversion to bits. Alias to 'unMemory'.
-}
toBits :: Memory mem -> Natural
toBits = coerce
{-# INLINE toBits #-}

-- | Lossless 'Memory' conversion to rational number.
toRat :: forall (mem :: Nat) . KnownNat mem => Memory mem -> Ratio Natural
toRat (Memory m) = m % nat @mem
{-# INLINE toRat #-}

{- | Floor 'Memory' unit to integral number. This function may lose some
information, so use only when:

1. You don't care about losing information.
2. You are sure that there will be no loss.
-}
floor
    :: forall (n :: Type) (mem :: Nat) .
       (Integral n, KnownNat mem)
    => Memory mem
    -> n
floor = Prelude.floor . toRat
{-# INLINE floor #-}
{-# SPECIALIZE floor :: KnownNat mem => Memory mem -> Int     #-}
{-# SPECIALIZE floor :: KnownNat mem => Memory mem -> Word    #-}
{-# SPECIALIZE floor :: KnownNat mem => Memory mem -> Integer #-}
{-# SPECIALIZE floor :: KnownNat mem => Memory mem -> Natural #-}

----------------------------------------------------------------------------
-- Numeric functions
----------------------------------------------------------------------------

{- | Returns the result of multiplication 'Natural with the given 'Memory value

>>> memoryMul 2 (byte 4)
Memory {unMemory = 64}
-}
memoryMul  :: Natural -> Memory mem -> Memory mem
memoryMul = stimes
{-# INLINE memoryMul #-}


{- | Returns the result of comparison of two 'Memory' values
and the difference between them as another 'Memory' of the same unit.

>>> memoryDiff (bit 4) (bit 8)
(LT,Memory {unMemory = 4})
>>> memoryDiff (byte 8) (byte 4)
(GT,Memory {unMemory = 32})
>>> :t it
it :: (Ordering, Memory Byte)
-}
memoryDiff :: Memory mem -> Memory mem -> (Ordering, Memory mem)
memoryDiff (Memory m1) (Memory m2) = case compare m1 m2 of
    LT -> (LT, Memory $ m2 - m1)
    GT -> (GT, Memory $ m1 - m2)
    EQ -> (EQ, Memory 0)
{-# INLINE memoryDiff #-}

{- | Returns the result of addition of two 'Memory' values casted
to the second memory unit.

>>> memoryPlus (bit 8) (megabite 2)
Memory {unMemory = 16000008}
-}
memoryPlus :: Memory mem1 -> Memory mem2 -> Memory mem2
memoryPlus m1 = (<>) (toMemory m1)
{-# INLINE memoryPlus #-}

{- | Retuns the reult of division of two 'Memory' values of any units.

>>> memoryDiv (kilobyte 3) (byte 2)
1500 % 1
-}
memoryDiv :: Memory mem1 -> Memory mem2 -> Ratio Natural
memoryDiv (Memory m1) (Memory m2) = m1 % m2
{-# INLINE memoryDiv #-}

----------------------------------------------------------------------------
-- Internal
----------------------------------------------------------------------------

nat :: forall (mem :: Nat) . KnownNat mem => Natural
nat = natVal (Proxy @mem)
{-# INLINE nat #-}
