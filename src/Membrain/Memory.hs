{-# LANGUAGE DataKinds #-}

-- | This module contains 'Memory' data type.

module Membrain.Memory
       ( Memory (..)
       ) where

import GHC.TypeNats (Nat)
import Numeric.Natural (Natural)


{- | Main memory units type. It has phantom parameter 'mem' of kind 'Nat' which
is type level representation of the unit.
-}
newtype Memory (mem :: Nat) = Memory
    { unMemory :: Natural
    } deriving (Show, Eq)
