{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
#if ( __GLASGOW_HASKELL__ >= 806 )
{-# LANGUAGE NoStarIsType           #-}
#endif

{- | This module contains type aliases for memory data units. According to the
official standard, there exist two naming conventions for memory units. Standard
measure prefixes are multipliers of @10@. But there are alternative prefixes
which end with @bi@ and they are represented as powers of @2@. The difference
can be displayed in the following table:

 +------------+----------------+----------+
 | Name       | Multipliers    | In bytes |
 +============+================+==========+
 | 'Kilobyte' | \( 10 ^ 3 \)   | 1000     |
 +------------+----------------+----------+
 | 'Kibibyte' | \( 2 ^ {10} \) | 1024     |
 +------------+----------------+----------+
 | 'Megabyte' | \( 10 ^ 6 \)   | 1000000  |
 +------------+----------------+----------+
 | 'Mebibyte' | \( 2 ^ {20} \) | 1048576  |
 +------------+----------------+----------+
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

         -- * Unit symbols
       , UnitSymbol
       , KnownUnitSymbol
       , unitSymbol
       ) where

import GHC.Exts (Proxy#, proxy#)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal')
import GHC.TypeNats (type (*), Nat)


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

{- | Type-level function to map memory units multipliers to proper symbols.

>>> :kind! UnitSymbol Bit
UnitSymbol Bit :: Symbol
= "b"
>>> :kind! UnitSymbol Byte
UnitSymbol Byte :: Symbol
= "B"
>>> :kind! UnitSymbol Kilobyte
UnitSymbol Kilobyte :: Symbol
= "kB"
>>> :kind! UnitSymbol Mebibyte
UnitSymbol Mebibyte :: Symbol
= "MiB"
-}
type family UnitSymbol (unit :: Nat) = (res :: Symbol) | res -> unit

type instance UnitSymbol 1 = "b"
type instance UnitSymbol 4 = "n"
type instance UnitSymbol 8 = "B"

type instance UnitSymbol 8000                      = "kB"
type instance UnitSymbol 8000000                   = "MB"
type instance UnitSymbol 8000000000                = "GB"
type instance UnitSymbol 8000000000000             = "TB"
type instance UnitSymbol 8000000000000000          = "PB"
type instance UnitSymbol 8000000000000000000       = "EB"
type instance UnitSymbol 8000000000000000000000    = "ZB"
type instance UnitSymbol 8000000000000000000000000 = "YB"

type instance UnitSymbol 8192                      = "KiB"
type instance UnitSymbol 8388608                   = "MiB"
type instance UnitSymbol 8589934592                = "GiB"
type instance UnitSymbol 8796093022208             = "TiB"
type instance UnitSymbol 9007199254740992          = "PiB"
type instance UnitSymbol 9223372036854775808       = "EiB"
type instance UnitSymbol 9444732965739290427392    = "ZiB"
type instance UnitSymbol 9671406556917033397649408 = "YiB"

-- | Constraint alias for 'KnownSymbol' calculated by 'UnitSymbol'.
type KnownUnitSymbol (mem :: Nat) = KnownSymbol (UnitSymbol mem)

{- | Term-level function to get value of the 'UnitSymbol' type family. Can be used
only with @-XTypeApplications@.

>>> unitSymbol @Terabyte
"TB"
>>> unitSymbol @Yobibyte
"YiB"
-}
unitSymbol :: forall (mem :: Nat) . KnownUnitSymbol mem => String
unitSymbol = symbolVal' (proxy# :: Proxy# (UnitSymbol mem))
{-# INLINE unitSymbol #-}
