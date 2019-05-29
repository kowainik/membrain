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
