{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE TypeInType #-}

-- | This module contains 'Memory' data type.

module Membrain.Memory
       ( -- * Data type
         Memory (..)
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


{- | Main memory units type. It has phantom parameter 'mem' of kind 'Nat' which
is type level representation of the unit.
-}
newtype Memory (mem :: Nat) = Memory
    { unMemory :: Natural
    } deriving (Show, Eq)

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
toRat (Memory m) = m % natVal (Proxy @mem)
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
