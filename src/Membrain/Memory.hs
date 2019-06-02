{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

{- | This module contains 'Memory' data type and various utility functions:

1. Create values of type 'Memory'.
2. Unwrap values of type 'Memory' to integral types.
3. Pretty-displaying functions.
4. Parsing.
5. Numeric functions.
-}

module Membrain.Memory
       ( -- * Data type
         Memory (..)
       , memory
       , toMemory
       , showMemory
       , readMemory

         -- * Conversion functions
       , toBits
       , toRat
       , floor

         -- * Numeric operations
       , memoryMul
       , memoryDiff
       , memoryPlus
       , memoryDiv

         -- * Any memory data type
         -- $any
       , AnyMemory (..)
       ) where

import Prelude hiding (floor)

import Data.Char (isDigit, isSpace)
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


{- $setup
>>> import Membrain
-}

{- | Main memory units type. It has phantom type parameter @mem@ of kind 'Nat'
which is type level representation of the unit. Stores internally memory as
bits. To construct values of type 'Memory', use functions from the
"Membrain.Constructors" module.
-}
newtype Memory (mem :: Nat) = Memory
    { unMemory :: Natural
    } deriving (Show, Read, Eq, Ord)

{- | Semigroup over addition.

>>> byte 2 <> byte 5
Memory {unMemory = 56}
-}
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

>>> showMemory (Memory 22 :: Memory Byte)
"2.75B"
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


{- | Inverse of 'showMemory'.

>>> readMemory @Byte "2.75B"
Just (Memory {unMemory = 22})
>>> readMemory @Bit "2.75B"
Nothing
-}
readMemory
    :: forall (mem :: Nat)
     . (KnownUnitSymbol mem, KnownNat mem)
    => String
    -> Maybe (Memory mem)
readMemory (dropWhile isSpace -> str) = case span isDigit str of
    ([], _) -> Nothing
    (_, []) -> Nothing
    (ds, '.': rest) -> case span isDigit rest of
        ([], _)           -> Nothing
        (numerator, unit) -> makeMemory ds numerator unit
    (ds, unit) -> makeMemory ds "0" unit
  where
    makeMemory :: String -> String -> String -> Maybe (Memory mem)
    makeMemory (read @Natural -> whole) numStr u =
        if unitSymbol @mem == u
        then case ((whole * numPow + num) * unit) `divMod` numPow of
            (b, 0) -> Just $ Memory b
            _      -> Nothing
        else Nothing
      where
          unit :: Natural
          unit = nat @mem

          num :: Natural
          num = read @Natural numStr

          numPow :: Natural
          numPow = 10 ^ length numStr

{- | Creates 'Memory' of unit @mem@ by the given 'Natural' number. 'Memory's
smart constructor.

>>> memory @Byte 3
Memory {unMemory = 24}
-}
memory :: forall (mem :: Nat) . KnownNat mem => Natural -> Memory mem
memory = Memory . (* nat @mem)
{-# INLINE memory #-}

{- | Convert memory from one unit to another.

__Note:__ this changes only view, not model.
So this operation has zero runtime cost.

>>> showMemory $ toMemory @Kilobyte $ byte 100
"0.1KB"
>>> showMemory $ toMemory @Kibibyte $ byte 100
"0.09765625KiB"
-}
toMemory :: forall (to :: Nat) (from :: Nat) . Memory from -> Memory to
toMemory = coerce
{-# INLINE toMemory #-}

{- | Lossless 'Memory' conversion to bits. Alias to 'unMemory'.

>>> toBits $ byte 1
8
>>> toBits $ kilobyte 1
8000
-}
toBits :: Memory mem -> Natural
toBits = coerce
{-# INLINE toBits #-}

{- | Lossless 'Memory' conversion to rational number.

>>> toRat $ byte 4
4 % 1
>>> toRat $ toMemory @Byte $ bit 22
11 % 4
-}
toRat :: forall (mem :: Nat) . KnownNat mem => Memory mem -> Ratio Natural
toRat (Memory m) = m % nat @mem
{-# INLINE toRat #-}

{- | Floor 'Memory' unit to integral number. This function may lose some
information, so use only when:

1. You don't care about losing information.
2. You are sure that there will be no loss.

>>> floor $ byte 4
4
>>> floor $ toMemory @Byte $ bit 22
2
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

{- | Returns the result of multiplication 'Natural' with the given 'Memory' value

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
>>> memoryDiff (kilobyte 2) (kilobyte 2)
(EQ,Memory {unMemory = 0})
-}
memoryDiff :: Memory mem -> Memory mem -> (Ordering, Memory mem)
memoryDiff (Memory m1) (Memory m2) = case compare m1 m2 of
    LT -> (LT, Memory $ m2 - m1)
    GT -> (GT, Memory $ m1 - m2)
    EQ -> (EQ, Memory 0)
{-# INLINE memoryDiff #-}

{- | Returns the result of addition of two 'Memory' values casted
to the second memory unit.

>>> memoryPlus (bit 8) (megabyte 2)
Memory {unMemory = 16000008}
-}
memoryPlus :: Memory mem1 -> Memory mem2 -> Memory mem2
memoryPlus m1 = (<>) (toMemory m1)
{-# INLINE memoryPlus #-}

{- | Retuns the result of division of two 'Memory' values of any units.

>>> memoryDiv (kilobyte 3) (byte 2)
1500 % 1
-}
memoryDiv :: Memory mem1 -> Memory mem2 -> Ratio Natural
memoryDiv (Memory m1) (Memory m2) = m1 % m2
{-# INLINE memoryDiv #-}

----------------------------------------------------------------------------
-- AnyMemory
----------------------------------------------------------------------------

{- $any
This data type is useful for working with 'Memory' of different units in
collections, or when 'Memory' of non-specified unit can be returned.
-}

-- | Existential data type for 'Memory's.
data AnyMemory
    = forall (mem :: Nat) . (KnownNat mem, KnownUnitSymbol mem)
    => MkAnyMemory (Memory mem)

instance Show AnyMemory where
    show (MkAnyMemory t) = showMemory t

----------------------------------------------------------------------------
-- Internal
----------------------------------------------------------------------------

nat :: forall (mem :: Nat) . KnownNat mem => Natural
nat = natVal (Proxy @mem)
{-# INLINE nat #-}
