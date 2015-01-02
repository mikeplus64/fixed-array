{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.Array.Fixed.Mutable
  ( MArray
    -- * Information
  , size, proxySize
    -- * Basic operations
    -- ** Reading
  , index, unsafeIndex
    -- ** Writing
  , write, unsafeWrite, set

    -- * Loops
  , for, for2

    -- * Creation
  , new, copy

     -- * Conversion
  , thaw, unsafeThaw
  , freeze, unsafeFreeze

    -- * `indices` re-exports
  , Z(..), (.:), Mode(..), Dim(toIndex,fromIndex), withRange, withRangeIndices
  )
  where
import           Control.Applicative
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe (unsafeInterleaveST)
import           Data.Array.Fixed.Base
import           Data.Index              hiding (size)
import qualified Data.Index              as Index
import           Foreign                 (Storable (..), peekElemOff,
                                          pokeElemOff, withForeignPtr)
import           GHC.ForeignPtr          (mallocPlainForeignPtrBytes)
import           System.IO.Unsafe        (unsafeInterleaveIO)

--------------------------------------------------------------------------------
-- Indexing

{-# INLINE index #-}
-- | Read from an index.
--
-- May throw 'IndexOutOfBounds' (an 'ArrayException'). This should not be a
-- performance concern, since generally GHC is able to see statically whether an
-- index is out of bounds.
index :: (Dim i, PrimMonad m, Storable a) => MArray (PrimState m) i a -> i
      -> m a
index marray ix = unsafeIndex marray (toIndex ix)

{-# INLINE unsafeIndex #-}
-- | Read directly from an index. Bounds are not checked.
unsafeIndex :: (PrimMonad m, Storable a) => MArray (PrimState m) i a -> Int
            -> m a
unsafeIndex (MArray fptr) ix =
  unsafePrimToPrim $
  withForeignPtr fptr (\ptr -> peekElemOff ptr ix)

--------------------------------------------------------------------------------
-- Writing

{-# INLINE write #-}
write :: (Dim i, PrimMonad m, Storable a)
      => MArray (PrimState m) i a -> i -> a -> m ()
write marray ix a = unsafeWrite marray (toIndex ix) a

{-# INLINE unsafeWrite #-}
-- | Write directly to an index. Bounds are not checked.
unsafeWrite
  :: (PrimMonad m, Storable a)
  => MArray (PrimState m) i a
  -> Int
  -> a
  -> m ()
unsafeWrite (MArray fptr) ix a =
  unsafePrimToPrim $
  withForeignPtr fptr (\ptr -> pokeElemOff ptr ix a)


{-# INLINE set #-}
-- | Set every element.
set :: forall m i a. (PrimMonad m, Applicative m, Storable a)
    => Mode i
    -> MArray (PrimState m) i a
    -> a
    -> m ()
set mode marray a = withRangeIndices mode (\ix -> unsafeWrite marray ix a)

--------------------------------------------------------------------------------
-- Creation

{-# INLINE new #-}
new :: forall m i a. (Dim i, Storable a, PrimMonad m)
    => Proxy i
    -> m (MArray (PrimState m) i a)
new s = unsafePrimToPrim $
  MArray `fmap`
  mallocPlainForeignPtrBytes (Index.size s * sizeOf (undefined :: a))

{-# INLINE copy #-}
copy :: (Storable a, Applicative m, PrimMonad m)
     => Mode i
     -> MArray (PrimState m) i a
     -> m (MArray (PrimState m) i a)
copy m marray = do
  marray' <- case m of
    Unroll -> new Proxy
    Roll   -> new Proxy

  withRangeIndices m (\ix ->
    unsafeWrite marray' ix =<< unsafeIndex marray ix)
  return marray'

--------------------------------------------------------------------------------
-- Loops

{-# INLINE for #-}
for
  :: (Applicative m, PrimMonad m, Storable a)
  => Mode i
  -> MArray (PrimState m) i a
  -> (Int -> a -> m ())
  -> m ()
for m arr f = withRangeIndices m $ \ix -> f ix =<< unsafeIndex arr ix

{-# INLINE for2 #-}
for2
  :: (Applicative m, PrimMonad m, Storable a)
  => Mode i
  -> MArray (PrimState m) i a
  -> MArray (PrimState m) i a
  -> (Int -> a -> a -> m ())
  -> m ()
for2 m arr0 arr1 f =
  withRangeIndices m $ \ix -> do
    e0 <- unsafeIndex arr0 ix
    e1 <- unsafeIndex arr1 ix
    f ix e0 e1
--------------------------------------------------------------------------------
-- Conversions

thaw :: (PrimMonad m, Applicative m, Storable a)
     => Mode i -> Array i a -> m (MArray (PrimState m) i a)
thaw m arr = copy m (unsafeThaw arr)

freeze
  :: (PrimMonad m, Applicative m, Storable a)
  => Mode i
  -> MArray (PrimState m) i a
  -> m (Array i a)
freeze m arr = unsafeFreeze <$> copy m arr

-- | Don't modify the result of this after calling this.
unsafeThaw :: Array i a -> MArray s i a
unsafeThaw (Array fptr) = MArray fptr

-- | Don't modify the parameter to this after calling this function.
unsafeFreeze :: MArray s i a -> Array i a
unsafeFreeze (MArray fptr) = Array fptr
