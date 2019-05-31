{- |
This modules introduces more type-safe interface of some standard function to
work with memory from @base@ package.
-}

module Membrain.Base
       ( -- * "System.IO" module
         hFileSize
       , hSetFileSize

         -- * "Data.Bits" module
       , bitSizeMaybe
       , finiteBitSize

         -- * "Foreign" module
       , sizeOf
       , allocaBytes
       , mallocBytes
       , callocBytes
       , reallocBytes
       , mallocForeignPtrBytes
       ) where

import Data.Bits (FiniteBits)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import Prelude hiding (floor)
import System.IO (Handle)

import Membrain.Constructors (bit, byte)
import Membrain.Memory (Memory, floor)
import Membrain.Units (Bit, Byte)

import qualified Data.Bits as Base (Bits, bitSizeMaybe, finiteBitSize)
import qualified Foreign.ForeignPtr as Base (mallocForeignPtrBytes)
import qualified Foreign.Marshal.Alloc as Base (allocaBytes, callocBytes, mallocBytes, reallocBytes)
import qualified Foreign.Storable as Base
import qualified System.IO as Base (hFileSize, hSetFileSize)


----------------------------------------------------------------------------
-- System.IO
----------------------------------------------------------------------------

{- | For a handle which attached to a physical file, the function returns the
size of that file in 'Byte's.

Similar 'Base.hFileSize' from @base@ but more type-safe: returns 'Memory Byte'
instead of 'Integer'.
-}
hFileSize :: Handle -> IO (Memory Byte)
hFileSize h = byte . fromInteger <$> Base.hFileSize h
{-# INLINE hFileSize #-}

{- | Truncates the physical file with the handle to the given size in 'Byte's.

Similar to 'Base.hSetFileSize' from @base@ but more type-safe: it works with
given 'Memory Byte' instead of 'Integer'.

__NOTE:__ This function makes 'floor' under the hood, so be careful when working
with the smaller units.
-}
hSetFileSize :: Handle -> Memory Byte -> IO ()
hSetFileSize h = Base.hSetFileSize h . floor
{-# INLINE hSetFileSize #-}

----------------------------------------------------------------------------
-- Data.Bits
----------------------------------------------------------------------------

{- | Returns the number of 'Bit's in the type of the argument. The actual value of
the argument is ignored. Returns 'Nothing' for types that do not have a fixed
bitsize, like 'Integer'.

Type safe version of the 'Base.bitSizeMaybe' function.

>>> bitSizeMaybe (0 :: Int)
Just (Memory {unMemory = 64})
>>> bitSizeMaybe (0 :: Integer)
Nothing
-}
bitSizeMaybe :: Base.Bits a => a -> Maybe (Memory Bit)
bitSizeMaybe x = bit . fromIntegral <$> Base.bitSizeMaybe x
{-# INLINE bitSizeMaybe #-}

{- | Return the number of bits in the type of the argument. The actual value of the argument is ignored.

Type safe version of the 'Base.finiteBitSize' function.

>>> finiteBitSize False
Memory {unMemory = 1}
>>> finiteBitSize (0 :: Int)
Memory {unMemory = 64}
-}
finiteBitSize :: FiniteBits b => b -> Memory Bit
finiteBitSize = bit . fromIntegral . Base.finiteBitSize
{-# INLINE finiteBitSize #-}

----------------------------------------------------------------------------
-- Foreign.Storable
----------------------------------------------------------------------------

{- | Like 'Base.sizeOf' but returns type-safe 'Memory Byte' instead of the 'Int'
which represents the amount of bytes.

Computes the storage requirements (in bytes) of the argument. The value of the
argument is not used.

>>> sizeOf True
Memory {unMemory = 32}
>>> sizeOf 'x'
Memory {unMemory = 32}
>>> sizeOf (0 :: Int)
Memory {unMemory = 64}
>>> sizeOf (0.0 :: Double)
Memory {unMemory = 64}
-}
sizeOf :: Storable a => a -> Memory Byte
sizeOf = byte . fromIntegral . Base.sizeOf
{-# INLINE sizeOf #-}

----------------------------------------------------------------------------
-- Foreign.Marshal.Alloc
----------------------------------------------------------------------------

{- | Executes the computation, passing as argument a pointer to a temporarily
allocated block of memory of @n@ 'Byte's. The block of memory is sufficiently
aligned for any of the basic foreign types that fits into a memory block of the
allocated size.

The memory is freed when the computation terminates (either normally or via
an exception), so the passed pointer must not be used after this.

Similar to 'Base.allocaBytes' but receives 'Byte's instead of 'Int'.
-}
allocaBytes :: Memory Byte -> (Ptr a -> IO b) -> IO b
allocaBytes bs = Base.allocaBytes (floor bs)
{-# INLINE allocaBytes #-}

{- | Allocates a block of memory of the given number of 'Byte's.
The block of memory is sufficiently aligned for any of the basic foreign types
that fits into a memory block of the allocated size.

Similar to 'Base.mallocBytes' but receives 'Byte's instead of 'Int'.
-}
mallocBytes :: Memory Byte -> IO (Ptr a)
mallocBytes = Base.mallocBytes . floor
{-# INLINE mallocBytes #-}

{- | Llike 'mallocBytes' but memory is filled with 'Byte's of value zero.
-}
callocBytes :: Memory Byte -> IO (Ptr a)
callocBytes = Base.callocBytes . floor
{-# INLINE callocBytes #-}

{- | Type safe version of the 'Base.reallocBytes' function. -}
reallocBytes :: Ptr a -> Memory Byte -> IO (Ptr a)
reallocBytes p = Base.reallocBytes p . floor
{-# INLINE reallocBytes #-}

----------------------------------------------------------------------------
-- Foreign.ForeignPtr
----------------------------------------------------------------------------

{- | Type safe version of the 'Base.mallocForeignPtrBytes' function. -}
mallocForeignPtrBytes :: Memory Byte -> IO (ForeignPtr a)
mallocForeignPtrBytes = Base.mallocForeignPtrBytes . floor
{-# INLINE mallocForeignPtrBytes #-}
