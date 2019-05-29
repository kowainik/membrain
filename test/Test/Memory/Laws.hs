module Test.Memory.Laws
       ( semigroupLaw
       , monoidIdentityLaw
       ) where

import Data.Semigroup (Semigroup (..))
import Hedgehog (MonadGen, Property, forAll, property, (===))
import Numeric.Natural (Natural)

import Membrain.Memory (Memory (..))
import Membrain.Units (Bit)

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

----------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------

-- | Generates random natural number up to 10^20.
genNatural :: (MonadGen m) => m Natural
genNatural = Gen.integral (Range.constant 0 $ 10 ^ (20 :: Int))

genBitMemory :: MonadGen m => m (Memory Bit)
genBitMemory = Memory <$> genNatural
