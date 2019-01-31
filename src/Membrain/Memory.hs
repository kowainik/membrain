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

         -- * Conversion functions
       , toBits
       , toRat
       , floor
       ) where

import Prelude hiding (floor)
import qualified Prelude

import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Ratio (Ratio, (%))
import GHC.TypeNats (KnownNat, Nat, natVal)
import Numeric.Natural (Natural)

import Membrain.Units (KnownUnitSymbol, unitSymbol)


{- | Main memory units type. It has phantom type parameter @mem@ of kind 'Nat'
which is type level representation of the unit.
-}
newtype Memory (mem :: Nat) = Memory
    { unMemory :: Natural
    } deriving (Eq)

{- | 'Show' instance for 'Memory' shows value as 'Double' with measure unit
suffix. It shows 'Memory' losslessly while used with standardized units of
measures. The following math fact is used to display 'Memory'.

A decimal representation written with a repeating final @0@ is said to terminate
before these zeros. Instead of @1.585000...@ one simply writes @1.585@. The
decimal is also called a terminating decimal. Terminating decimals represent
rational numbers of the form \( \cfrac{k}{2^n 5^m} \). If you use units of the
different form then the 'show' function for 'Memory' hangs. TODO: check this
statically
-}
instance (KnownNat mem, KnownUnitSymbol mem) => Show (Memory mem) where
    show :: Memory mem -> String
    show (Memory m) = showFrac m (nat @mem) ++ unitSymbol @mem
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

{- | Convert memory from one unit to another. __Note:__ this changes only view,
not model. So this operation has zero runtime cost.
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
-- Internal
----------------------------------------------------------------------------

nat :: forall (mem :: Nat) . KnownNat mem => Natural
nat = natVal (Proxy @mem)
{-# INLINE nat #-}
