module Main (main) where

import Hedgehog (Group (..), checkParallel)
import System.Exit (exitFailure, exitSuccess)

import Test.Memory.Laws (monoidIdentityLaw, semigroupLaw)
import Test.Memory.TypeLevel (runTypeLevelTests)


main :: IO ()
main = do
    runTypeLevelTests
    checkParallel hedgehogTests >>= \p -> if p then exitSuccess else exitFailure

hedgehogTests :: Group
hedgehogTests = Group "Roundtrip properties"
    [ semigroupLaw      `named` "Semigroup: x <> (y <> z) ≡ (x <> y) <> z"
    , monoidIdentityLaw `named` "Monoid Identity: x <> mempty ≡ x"
    ]
  where
    named :: a -> b -> (b, a)
    named = flip (,)
