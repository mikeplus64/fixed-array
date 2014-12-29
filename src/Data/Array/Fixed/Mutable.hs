{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NullaryTypeClasses  #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Data.Array.Fixed.Mutable where
import           Control.Applicative
import           Control.Monad.Primitive
import           Data.Index              hiding (size)
import qualified Data.Index              as Index
import           Foreign.ForeignPtr
import           Foreign.Storable
import           GHC.ForeignPtr          (mallocPlainForeignPtrBytes)

-- | An @Array i a@ is an array with indices bounded by @i@.
newtype MArray s i a = MArray (ForeignPtr a)
newtype Array i a = Array (ForeignPtr a)
type role MArray nominal representational representational
type role Array representational representational

--------------------------------------------------------------------------------
-- Utility

{-# INLINE size #-}
size :: forall array i a. Dim i => array i a -> Int
size _ = Index.size (Proxy :: Proxy i)

{-# INLINE proxySize #-}
proxySize :: Dim i => array i a -> Proxy i
proxySize _ = Proxy

{-# INLINE withSize #-}
withSize :: array i a -> (Proxy i -> b)  -> b
withSize _ f = f Proxy

--------------------------------------------------------------------------------
-- Indexing

-- If we want to be really devious... We could add
-- {-# RULES "out of bounds"
--       forall i s. outOfBounds i s = outOfBounds s i #-}
-- Then you'll get a ghc panic whenever you index something wrong! And probably
-- sometimes when you didn't. :)

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

--------------------------------------------------------------------------------
-- Helpers
