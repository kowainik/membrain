{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Type-safe memory units.

@membrain@ aims for qualified imports. Import as follows:

@
__import__ __qualified__ Membrain __as__ Mem
@
-}

module Membrain
       ( -- $mem
         module Membrain.Memory
         -- $units
       , module Membrain.Units
         -- $constructors
       , module Membrain.Constructors
         -- $base
       , module Membrain.Base
       ) where

import Membrain.Base
import Membrain.Constructors
import Membrain.Memory
import Membrain.Units

{- $mem
Main 'Memory' data type with many utility functions.
-}

{- $units
Type-level unit multipliers.
-}

{- $constructors
Smart constructors for creating values of type 'Memory'.
-}

{- $base
Type-safe versions of functions from @base@ that work with memory-related
values.
-}
