{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Type-safe memory units. This package has the following structure:

* __"Membrain.Memory":__ main 'Memory' data type with many utility functions.
* __"Membrain.Units":__ type-level unit multipliers.
* __"Membrain.Constructors":__ smart constructors for creating values of type
  'Memory'.
* __"Membrain.Base":__ type-safe versions of functions from @base@ that work
  with memory-related values.

@membrain@ aims for qualified imports. Import as follows:

@
__import__ __qualified__ Membrain __as__ Mem
@
-}

module Membrain
       ( module Membrain.Base
       , module Membrain.Constructors
       , module Membrain.Memory
       , module Membrain.Units
       ) where

import Membrain.Base
import Membrain.Constructors
import Membrain.Memory
import Membrain.Units
