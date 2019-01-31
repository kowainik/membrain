{- | This module implements smart constructors for creating values of type
'Memory'.
-}

module Membrain.Constructors
       ( -- * Smart constructors
         bit
       , nibble
       , byte

       , kilobyte
       , megabyte
       , gigabyte
       , terabyte
       , petabyte
       , exabyte
       , zettabyte
       , yottabyte

       , kibibyte
       , mebibyte
       , gibibyte
       , tebibyte
       , pebibyte
       , exbibyte
       , zebibyte
       , yobibyte
       ) where

import GHC.Natural (Natural)

import Membrain.Memory (Memory (..), memory)
import Membrain.Units (Bit, Byte, Exabyte, Exbibyte, Gibibyte, Gigabyte, Kibibyte, Kilobyte,
                       Mebibyte, Megabyte, Nibble, Pebibyte, Petabyte, Tebibyte, Terabyte, Yobibyte,
                       Yottabyte, Zebibyte, Zettabyte)


-- | Creates 'Memory' in 'Bit's from given 'Natural' as the number of bits.
bit :: Natural -> Memory Bit
bit = Memory
{-# INLINE bit #-}

-- | Creates 'Memory' in 'Nibble's from given 'Natural' as the number of nibbles.
nibble :: Natural -> Memory Nibble
nibble = memory
{-# INLINE nibble #-}

-- | Creates 'Memory' in 'Byte's from given 'Natural' as the number of bytes.
byte :: Natural -> Memory Byte
byte = memory
{-# INLINE byte #-}

-- | Creates 'Memory' in 'Kilobyte's from given 'Natural' as the number of kilobytes.
kilobyte :: Natural -> Memory Kilobyte
kilobyte = memory
{-# INLINE kilobyte #-}

-- | Creates 'Memory' in 'Megabyte's from given 'Natural' as the number of megabytes.
megabyte :: Natural -> Memory Megabyte
megabyte = memory
{-# INLINE megabyte #-}

-- | Creates 'Memory' in 'Gigabyte's from given 'Natural' as the number of gigabytes.
gigabyte :: Natural -> Memory Gigabyte
gigabyte = memory
{-# INLINE gigabyte #-}

-- | Creates 'Memory' in 'Terabyte's from given 'Natural' as the number of terabytes.
terabyte :: Natural -> Memory Terabyte
terabyte = memory
{-# INLINE terabyte #-}

-- | Creates 'Memory' in 'Petabyte's from given 'Natural' as the number of petabytes.
petabyte :: Natural -> Memory Petabyte
petabyte = memory
{-# INLINE petabyte #-}

-- | Creates 'Memory' in 'Exabyte's from given 'Natural' as the number of exabytes.
exabyte :: Natural -> Memory Exabyte
exabyte = memory
{-# INLINE exabyte #-}

-- | Creates 'Memory' in 'Zettabyte's from given 'Natural' as the number of zettabytes.
zettabyte :: Natural -> Memory Zettabyte
zettabyte = memory
{-# INLINE zettabyte #-}

-- | Creates 'Memory' in 'Yottabyte's from given 'Natural' as the number of yottabytes.
yottabyte :: Natural -> Memory Yottabyte
yottabyte = memory
{-# INLINE yottabyte #-}

-- | Creates 'Memory' in 'Kibibyte's from given 'Natural' as the number of kibibytes.
kibibyte :: Natural -> Memory Kibibyte
kibibyte = memory
{-# INLINE kibibyte #-}

-- | Creates 'Memory' in 'Mebibyte's from given 'Natural' as the number of mebibytes.
mebibyte :: Natural -> Memory Mebibyte
mebibyte = memory
{-# INLINE mebibyte #-}

-- | Creates 'Memory' in 'Gibibyte's from given 'Natural' as the number of gibibytes.
gibibyte :: Natural -> Memory Gibibyte
gibibyte = memory
{-# INLINE gibibyte #-}

-- | Creates 'Memory' in 'Tebibyte's from given 'Natural' as the number of tebibytes.
tebibyte :: Natural -> Memory Tebibyte
tebibyte = memory
{-# INLINE tebibyte #-}

-- | Creates 'Memory' in 'Pebibyte's from given 'Natural' as the number of pebibytes.
pebibyte :: Natural -> Memory Pebibyte
pebibyte = memory
{-# INLINE pebibyte #-}

-- | Creates 'Memory' in 'Exbibyte's from given 'Natural' as the number of exbibytes.
exbibyte :: Natural -> Memory Exbibyte
exbibyte = memory
{-# INLINE exbibyte #-}

-- | Creates 'Memory' in 'Zebibyte's from given 'Natural' as the number of zebibytes.
zebibyte :: Natural -> Memory Zebibyte
zebibyte = memory
{-# INLINE zebibyte #-}

-- | Creates 'Memory' in 'Yobibyte's from given 'Natural' as the number of yobibytes.
yobibyte :: Natural -> Memory Yobibyte
yobibyte = memory
{-# INLINE yobibyte #-}
