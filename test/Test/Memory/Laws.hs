module Test.Memory.Laws
       ( semigroupLaw
       , monoidIdentityLaw
       , showReadLaw
       ) where

import Data.Semigroup (Semigroup (..))
import Hedgehog (MonadGen, Property, forAll, property, (===))
import Numeric.Natural (Natural)

import Membrain.Memory (AnyMemory (..), Memory (..), readMemory, showMemory)

import qualified Membrain.Units as Mem

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


----------------------------------------------------------------------------
-- Laws
----------------------------------------------------------------------------

{- | The semigroup associativity axiom:

@
x <> (y <> z) ≡ (x <> y) <> z
@
-}
semigroupLaw :: Property
semigroupLaw = property $ do
    x <- forAll genBitMemory
    y <- forAll genBitMemory
    z <- forAll genBitMemory
    (x <> y) <> z === x <> (y <> z)

{- | Identity law for Monoid

@
mempty <> x = x
x <> mempty = x
@
-}
monoidIdentityLaw :: Property
monoidIdentityLaw = property $ do
    x <- forAll genBitMemory

    x <> mempty === x
    mempty <> x === x

showReadLaw :: Property
showReadLaw = property $ do
    MkAnyMemory mem <- forAll genAnyMemory
    readMemory (showMemory mem) === Just mem

----------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------

-- | Generates random natural number up to 10^20.
genNatural :: (MonadGen m) => m Natural
genNatural = Gen.integral (Range.constant 0 $ 10 ^ (20 :: Int))

genBitMemory :: MonadGen m => m (Memory Mem.Bit)
genBitMemory = Memory <$> genNatural

genAnyMemory :: MonadGen m => m AnyMemory
genAnyMemory = genNatural >>= unitChooser

-- | Returns random 'AnyMemory'.
unitChooser :: (MonadGen m) => Natural -> m AnyMemory
unitChooser n = Gen.element
    [ MkAnyMemory (Memory @Mem.Bit       n)
    , MkAnyMemory (Memory @Mem.Nibble    n)
    , MkAnyMemory (Memory @Mem.Byte      n)

    , MkAnyMemory (Memory @Mem.Kilobyte  n)
    , MkAnyMemory (Memory @Mem.Megabyte  n)
    , MkAnyMemory (Memory @Mem.Gigabyte  n)
    , MkAnyMemory (Memory @Mem.Terabyte  n)
    , MkAnyMemory (Memory @Mem.Petabyte  n)
    , MkAnyMemory (Memory @Mem.Exabyte   n)
    , MkAnyMemory (Memory @Mem.Zettabyte n)
    , MkAnyMemory (Memory @Mem.Yottabyte n)

    , MkAnyMemory (Memory @Mem.Kibibyte  n)
    , MkAnyMemory (Memory @Mem.Mebibyte  n)
    , MkAnyMemory (Memory @Mem.Gibibyte  n)
    , MkAnyMemory (Memory @Mem.Tebibyte  n)
    , MkAnyMemory (Memory @Mem.Pebibyte  n)
    , MkAnyMemory (Memory @Mem.Exbibyte  n)
    , MkAnyMemory (Memory @Mem.Zebibyte  n)
    , MkAnyMemory (Memory @Mem.Yobibyte  n)
    ]
