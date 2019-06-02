-- | All unit tests for runtime values and functions.

module Test.Memory.ValueLevel
       ( unitTests
       ) where

import Data.Ratio ((%))
import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Membrain as Mem


unitTests :: Spec
unitTests = describe "Membrain: Unit tests" $ do
    unitConversionSpec
    unwrappingSpec
    readMemorySpec

unitConversionSpec :: Spec
unitConversionSpec = describe "toMemory" $ do
    it "converting to the same unit does nothing" $
        Mem.byte 42 `shouldBe` Mem.toMemory @Mem.Byte (Mem.byte 42)
    it "converting to the another unit" $
        Mem.byte 1 `shouldBe` Mem.toMemory @Mem.Byte (Mem.bit 8)

unwrappingSpec :: Spec
unwrappingSpec = describe "fromMemory" $ do
    it "toBits from bits" $
        Mem.toBits (Mem.bit 42) `shouldBe` 42
    it "toBits from bytes" $
        Mem.toBits (Mem.byte 2) `shouldBe` 16
    it "toRat from bits" $
        Mem.toRat (Mem.toMemory @Mem.Byte $ Mem.bit 7) `shouldBe` (7 % 8)
    it "flooring bytes" $
        Mem.floor @Int (Mem.byte 2) `shouldBe` 2
    it "flooring bytes from bits" $
        Mem.floor @Int (Mem.toMemory @Mem.Byte $ Mem.bit 15) `shouldBe` 1

readMemorySpec :: Spec
readMemorySpec = describe "readMemory" $ do
    it "reads correct integral Byte" $
        Mem.readMemory @Mem.Byte "2B" `shouldBe` Just (Mem.byte 2)
    it "reads correct integral with '.' Byte" $
        Mem.readMemory @Mem.Byte "2.75B" `shouldBe` Just (Mem.Memory 22)
    it "doesn't read non-representable amount of Bytes" $
        Mem.readMemory @Mem.Byte "2.22B" `shouldBe` Nothing
    it "doesn't read if type and unit symbol don't match" $
        Mem.readMemory @Mem.Terabyte "42MB" `shouldBe` Nothing
    it "doesn't read if whole part is missing" $
        Mem.readMemory @Mem.Megabyte ".42MB" `shouldBe` Nothing
    it "reads correct integral Kibibyte" $
        Mem.readMemory @Mem.Kibibyte "42KiB" `shouldBe` Just (Mem.kibibyte 42)
    it "reads correct integral Kibibyte" $
        Mem.readMemory @Mem.Kibibyte "42.5KiB" `shouldBe` Just (Mem.Memory 348160)
    it "doesn't read string with letters" $
        Mem.readMemory @Mem.Megabyte "1a.42MB" `shouldBe` Nothing
    it "doesn't read string with double dots" $
        Mem.readMemory @Mem.Megabyte "1.4.2MB" `shouldBe` Nothing
    it "doesn't read string without unit" $
        Mem.readMemory @Mem.Megabyte "42" `shouldBe` Nothing
