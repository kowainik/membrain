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


{- $setup
>>> import Membrain
-}

{- | Creates 'Memory' in 'Bit's from given 'Natural' as the number of bits.

>>> showMemory $ bit 42
"42b"
-}
bit :: Natural -> Memory Bit
bit = Memory
{-# INLINE bit #-}

{- | Creates 'Memory' in 'Nibble's from given 'Natural' as the number of nibbles.

>>> showMemory $ nibble 42
"42n"
-}
nibble :: Natural -> Memory Nibble
nibble = memory
{-# INLINE nibble #-}

{- | Creates 'Memory' in 'Byte's from given 'Natural' as the number of bytes.

>>> showMemory $ byte 42
"42B"
-}
byte :: Natural -> Memory Byte
byte = memory
{-# INLINE byte #-}

{- | Creates 'Memory' in 'Kilobyte's from given 'Natural' as the number of kilobytes.

>>> showMemory $ kilobyte 42
"42KB"
-}
kilobyte :: Natural -> Memory Kilobyte
kilobyte = memory
{-# INLINE kilobyte #-}

{- | Creates 'Memory' in 'Megabyte's from given 'Natural' as the number of megabytes.

>>> showMemory $ megabyte 42
"42MB"
-}
megabyte :: Natural -> Memory Megabyte
megabyte = memory
{-# INLINE megabyte #-}

{- | Creates 'Memory' in 'Gigabyte's from given 'Natural' as the number of gigabytes.

>>> showMemory $ gigabyte 42
"42GB"
-}
gigabyte :: Natural -> Memory Gigabyte
gigabyte = memory
{-# INLINE gigabyte #-}

{- | Creates 'Memory' in 'Terabyte's from given 'Natural' as the number of terabytes.

>>> showMemory $ terabyte 42
"42TB"
-}
terabyte :: Natural -> Memory Terabyte
terabyte = memory
{-# INLINE terabyte #-}

{- | Creates 'Memory' in 'Petabyte's from given 'Natural' as the number of petabytes.

>>> showMemory $ petabyte 42
"42PB"
-}
petabyte :: Natural -> Memory Petabyte
petabyte = memory
{-# INLINE petabyte #-}

{- | Creates 'Memory' in 'Exabyte's from given 'Natural' as the number of exabytes.

>>> showMemory $ exabyte 42
"42EB"
-}
exabyte :: Natural -> Memory Exabyte
exabyte = memory
{-# INLINE exabyte #-}

{- | Creates 'Memory' in 'Zettabyte's from given 'Natural' as the number of zettabytes.

>>> showMemory $ zettabyte 42
"42ZB"
-}
zettabyte :: Natural -> Memory Zettabyte
zettabyte = memory
{-# INLINE zettabyte #-}

{- | Creates 'Memory' in 'Yottabyte's from given 'Natural' as the number of yottabytes.

>>> showMemory $ yottabyte 42
"42YB"
-}
yottabyte :: Natural -> Memory Yottabyte
yottabyte = memory
{-# INLINE yottabyte #-}

{- | Creates 'Memory' in 'Kibibyte's from given 'Natural' as the number of kibibytes.

>>> showMemory $ kibibyte 42
"42KiB"
-}
kibibyte :: Natural -> Memory Kibibyte
kibibyte = memory
{-# INLINE kibibyte #-}

{- | Creates 'Memory' in 'Mebibyte's from given 'Natural' as the number of mebibytes.

>>> showMemory $ mebibyte 42
"42MiB"
-}
mebibyte :: Natural -> Memory Mebibyte
mebibyte = memory
{-# INLINE mebibyte #-}

{- | Creates 'Memory' in 'Gibibyte's from given 'Natural' as the number of gibibytes.

>>> showMemory $ gibibyte 42
"42GiB"
-}
gibibyte :: Natural -> Memory Gibibyte
gibibyte = memory
{-# INLINE gibibyte #-}

{- | Creates 'Memory' in 'Tebibyte's from given 'Natural' as the number of tebibytes.

>>> showMemory $ tebibyte 42
"42TiB"
-}
tebibyte :: Natural -> Memory Tebibyte
tebibyte = memory
{-# INLINE tebibyte #-}

{- | Creates 'Memory' in 'Pebibyte's from given 'Natural' as the number of pebibytes.

>>> showMemory $ pebibyte 42
"42PiB"
-}
pebibyte :: Natural -> Memory Pebibyte
pebibyte = memory
{-# INLINE pebibyte #-}

{- | Creates 'Memory' in 'Exbibyte's from given 'Natural' as the number of exbibytes.

>>> showMemory $ exbibyte 42
"42EiB"
-}
exbibyte :: Natural -> Memory Exbibyte
exbibyte = memory
{-# INLINE exbibyte #-}

{- | Creates 'Memory' in 'Zebibyte's from given 'Natural' as the number of zebibytes.

>>> showMemory $ zebibyte 42
"42ZiB"
-}
zebibyte :: Natural -> Memory Zebibyte
zebibyte = memory
{-# INLINE zebibyte #-}

{- | Creates 'Memory' in 'Yobibyte's from given 'Natural' as the number of yobibytes.

>>> showMemory $ yobibyte 42
"42YiB"
-}
yobibyte :: Natural -> Memory Yobibyte
yobibyte = memory
{-# INLINE yobibyte #-}
