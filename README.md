# membrain

[![Build status](https://img.shields.io/travis/kowainik/membrain.svg?logo=travis)](https://travis-ci.org/kowainik/membrain)
[![Hackage](https://img.shields.io/hackage/v/membrain.svg?logo=haskell)](https://hackage.haskell.org/package/membrain)
[![Stackage Lts](http://stackage.org/package/membrain/badge/lts)](http://stackage.org/lts/package/membrain)
[![Stackage Nightly](http://stackage.org/package/membrain/badge/nightly)](http://stackage.org/nightly/package/membrain)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

This package implements type-safe memory units. It's build around the following
data type:

```haskell
newtype Memory (mem :: Nat) = Memory
    { unMemory :: Natural
    }
```

This data type stores every memory internally as bits. However, unit multiplier
is stored as type-level natural number. This approach allows to represent
different units with low amount of boilerplate.

`membrain` implements various useful functions to work with `Memory`: smart
constructors, conversion functions, pretty displaying, parsing, numeric
functions, type-safe wrappers around functions from `base`.
