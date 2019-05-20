{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Test.Memory.TypeLevel
       ( runTypeLevelTests
       ) where

import Test.TypeSpec (Is, It, TypeSpec (Valid))
import Test.TypeSpecCrazy (type ( ### ), type (-*-), type (-/-), type (~~~))

import Membrain.Units

-- | Type-check and execute all type-level tests.
runTypeLevelTests :: IO ()
runTypeLevelTests = print unitSymbolTests

unitSymbolTests ::
    "UnitSymbol"
    ###

        "Simple"
        ~~~
            It "Name Bit    = b" (UnitSymbol Bit    `Is` "b")
        -*- It "Name Nibble = n" (UnitSymbol Nibble `Is` "n")
        -*- It "Name Byte   = B" (UnitSymbol Byte   `Is` "B")

    -/-

        "Decimal"
        ~~~
            It "Name Kilobyte  = KB" (UnitSymbol Kilobyte  `Is` "KB")
        -*- It "Name Megabyte  = MB" (UnitSymbol Megabyte  `Is` "MB")
        -*- It "Name Gigabyte  = GB" (UnitSymbol Gigabyte  `Is` "GB")
        -*- It "Name Terabyte  = TB" (UnitSymbol Terabyte  `Is` "TB")
        -*- It "Name Petabyte  = PB" (UnitSymbol Petabyte  `Is` "PB")
        -*- It "Name Exabyte   = EB" (UnitSymbol Exabyte   `Is` "EB")
        -*- It "Name Zettabyte = ZB" (UnitSymbol Zettabyte `Is` "ZB")
        -*- It "Name Yottabyte = YB" (UnitSymbol Yottabyte `Is` "YB")

    -/-

        "Binary"
        ~~~
            It "Name Kibibyte = KiB" (UnitSymbol Kibibyte `Is` "KiB")
        -*- It "Name Mebibyte = MiB" (UnitSymbol Mebibyte `Is` "MiB")
        -*- It "Name Gibibyte = GiB" (UnitSymbol Gibibyte `Is` "GiB")
        -*- It "Name Tebibyte = TiB" (UnitSymbol Tebibyte `Is` "TiB")
        -*- It "Name Pebibyte = PiB" (UnitSymbol Pebibyte `Is` "PiB")
        -*- It "Name Exbibyte = EiB" (UnitSymbol Exbibyte `Is` "EiB")
        -*- It "Name Zebibyte = ZiB" (UnitSymbol Zebibyte `Is` "ZiB")
        -*- It "Name Yobibyte = YiB" (UnitSymbol Yobibyte `Is` "YiB")
unitSymbolTests = Valid
