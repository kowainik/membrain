module Main (main) where

import Hedgehog (Group (..), checkParallel)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Test.Hspec (hspec)

import Test.Memory.Laws (monoidIdentityLaw, semigroupLaw, showReadLaw)
import Test.Memory.TypeLevel (runTypeLevelTests)
import Test.Memory.ValueLevel (unitTests)


main :: IO ()
main = do
    -- fix terminal encoding
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    runTypeLevelTests
    hspec unitTests
    checkParallel hedgehogTests >>= \p -> if p then exitSuccess else exitFailure

hedgehogTests :: Group
hedgehogTests = Group "Roundtrip properties"
    [ semigroupLaw      `named` "Semigroup: x <> (y <> z) ≡ (x <> y) <> z"
    , monoidIdentityLaw `named` "Monoid Identity: x <> mempty ≡ x"
    , showReadLaw       `named` "read . show x ≡ Just x"
    ]
  where
    named :: a -> b -> (b, a)
    named = flip (,)
