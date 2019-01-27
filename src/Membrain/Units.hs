{-# LANGUAGE CPP           #-}
{-# LANGUAGE DataKinds     #-}
#if ( __GLASGOW_HASKELL__ >= 806 )
{-# LANGUAGE NoStarIsType  #-}
#endif
{-# LANGUAGE TypeOperators #-}

{- | This module contains type aliases for memory data units.

TODO: explain kilo/kibi difference.

-}

module Membrain.Units
       ( -- * Simple
         Bit
       , Nibble
       , Byte

         -- * Decimal
       , Kilobyte
       , Megabyte
       , Gigabyte
       , Terabyte
       , Petabyte
       , Exabyte
       , Zettabyte
       , Yottabyte

         -- * Binary
       , Kibibyte
       , Mebibyte
       , Gibibyte
       , Tebibyte
       , Pebibyte
       , Exbibyte
       , Zebibyte
       , Yobibyte

         -- * Smart constructors
       , bit
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
import GHC.TypeNats (type (*))

import Membrain.Memory (Memory, memory)


type Bit       = 1
type Nibble    = 4
type Byte      = 8

type Kilobyte  = 1000 * Byte
type Megabyte  = 1000 * Kilobyte
type Gigabyte  = 1000 * Megabyte
type Terabyte  = 1000 * Gigabyte
type Petabyte  = 1000 * Terabyte
type Exabyte   = 1000 * Petabyte
type Zettabyte = 1000 * Exabyte
type Yottabyte = 1000 * Zettabyte

type Kibibyte  = 1024 * Byte
type Mebibyte  = 1024 * Kibibyte
type Gibibyte  = 1024 * Mebibyte
type Tebibyte  = 1024 * Gibibyte
type Pebibyte  = 1024 * Tebibyte
type Exbibyte  = 1024 * Pebibyte
type Zebibyte  = 1024 * Exbibyte
type Yobibyte  = 1024 * Zebibyte

-- | Creates 'Memory' in 'Bit's from given 'Natural' as the number of bits.
bit :: Natural -> Memory Bit
bit = memory

-- | Creates 'Memory' in 'Nibble's from given 'Natural' as the number of nibbles.
nibble :: Natural -> Memory Nibble
nibble = memory

-- | Creates 'Memory' in 'Byte's from given 'Natural' as the number of bytes.
byte :: Natural -> Memory Byte
byte = memory

-- | Creates 'Memory' in 'Kilobyte's from given 'Natural' as the number of kilobytes.
kilobyte :: Natural -> Memory Kilobyte
kilobyte = memory

-- | Creates 'Memory' in 'Megabyte's from given 'Natural' as the number of megabytes.
megabyte :: Natural -> Memory Megabyte
megabyte = memory

-- | Creates 'Memory' in 'Gigabyte's from given 'Natural' as the number of gigabytes.
gigabyte :: Natural -> Memory Gigabyte
gigabyte = memory

-- | Creates 'Memory' in 'Terabyte's from given 'Natural' as the number of terabytes.
terabyte :: Natural -> Memory Terabyte
terabyte = memory

-- | Creates 'Memory' in 'Petabyte's from given 'Natural' as the number of petabytes.
petabyte :: Natural -> Memory Petabyte
petabyte = memory

-- | Creates 'Memory' in 'Exabyte's from given 'Natural' as the number of exabytes.
exabyte :: Natural -> Memory Exabyte
exabyte = memory

-- | Creates 'Memory' in 'Zettabyte's from given 'Natural' as the number of zettabytes.
zettabyte :: Natural -> Memory Zettabyte
zettabyte = memory

-- | Creates 'Memory' in 'Yottabyte's from given 'Natural' as the number of yottabytes.
yottabyte :: Natural -> Memory Yottabyte
yottabyte = memory

-- | Creates 'Memory' in 'Kibibyte's from given 'Natural' as the number of kibibytes.
kibibyte :: Natural -> Memory Kibibyte
kibibyte = memory

-- | Creates 'Memory' in 'Mebibyte's from given 'Natural' as the number of mebibytes.
mebibyte :: Natural -> Memory Mebibyte
mebibyte = memory

-- | Creates 'Memory' in 'Gibibyte's from given 'Natural' as the number of gibibytes.
gibibyte :: Natural -> Memory Gibibyte
gibibyte = memory

-- | Creates 'Memory' in 'Tebibyte's from given 'Natural' as the number of tebibytes.
tebibyte :: Natural -> Memory Tebibyte
tebibyte = memory

-- | Creates 'Memory' in 'Pebibyte's from given 'Natural' as the number of pebibytes.
pebibyte :: Natural -> Memory Pebibyte
pebibyte = memory

-- | Creates 'Memory' in 'Exbibyte's from given 'Natural' as the number of exbibytes.
exbibyte :: Natural -> Memory Exbibyte
exbibyte = memory

-- | Creates 'Memory' in 'Zebibyte's from given 'Natural' as the number of zebibytes.
zebibyte :: Natural -> Memory Zebibyte
zebibyte = memory

-- | Creates 'Memory' in 'Yobibyte's from given 'Natural' as the number of yobibytes.
yobibyte :: Natural -> Memory Yobibyte
yobibyte = memory
