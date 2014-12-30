{-# LANGUAGE ScopedTypeVariables #-}
module Data.Array.Fixed.Mutable where
import Control.Applicative
import Control.Monad.Primitive
import Data.Array.Fixed.Base
import Data.Index              as Index
import Foreign                 (Storable (..), peekElemOff, pokeElemOff,
                                withForeignPtr)
import GHC.ForeignPtr          (mallocPlainForeignPtrBytes)

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

new :: forall m i a. (Dim i, Storable a, PrimMonad m)
    => Proxy i
    -> m (MArray (PrimState m) i a)
new s = unsafePrimToPrim $
  MArray `fmap`
  mallocPlainForeignPtrBytes (Index.size s * sizeOf (undefined :: a))

copy :: forall m i a. (Storable a, PrimMonad m)
     => Mode i
     -> MArray (PrimState m) i a
     -> m (MArray (PrimState m) i a)
copy m marray = do
  marray' <- new (case m of Unroll -> Proxy; Roll -> Proxy) -- get Dim instance
  withRangeIndices m (\ix ->
    unsafeWrite marray' ix =<< unsafeIndex marray ix)
  return marray'

elems :: m (MArray (PrimState m) i a) -> a -> m (MArray (PrimState m) i a)
elems a _ = a

--------------------------------------------------------------------------------
-- Manipulation

forMArray
  :: (Applicative m, PrimMonad m, Storable a)
  => MArray (PrimState m) i a
  -> Mode i
  -> (Int -> a -> m ())
  -> m ()
forMArray arr m f = forArrayIx arr m (\ix -> f ix =<< unsafeIndex arr ix)

