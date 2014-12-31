{-# LANGUAGE BangPatterns               #-}
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
  , mfor

    -- * Creation
  , new, copy
    -- ** Monadic creation interface
  , Create
  , create
  , freezeIO
  , freezeST
    -- *** Basic operations
    -- **** Writing
  , (<~), (!<~), mset
  , mload
  , mloadM
  , mloadList
    -- *** Type hints
  , msize, mof

    -- * Conversion
  , thaw, unsafeThaw
  , freeze, unsafeFreeze

    -- * `indices` re-exports
  , Z(..), (:.)(..), (.:), Mode(..), dim, dimr, dimu, Dim(toIndex,fromIndex)
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

{-# INLINE mfor #-}
mfor
  :: (Applicative m, PrimMonad m, Storable a)
  => Mode i
  -> (Int -> Create i a m ())
  -> Create i a m ()
mfor m f = withRangeIndices m f

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
-- Monadic creation interface

newtype Create dim e m a = Create (ReaderT (MArray (PrimState m) dim e) m a)
  deriving (Monad, Functor, Applicative)

deriving instance
  (PrimMonad m, s ~ PrimState m) => MonadReader (MArray s n e) (Create n e m)

deriving instance MonadIO (Create dim e IO)
instance MonadTrans (Create dim e) where lift = Create . lift

{-# INLINE freezeST #-}
freezeST :: (Storable e, Dim i) => Create i e (ST s) () -> ST s (Array i e)
freezeST (Create c) = do
  arr <- new Proxy
  runReaderT c arr
  return (unsafeFreeze arr)

{-# INLINE create #-}
-- | Create an array from a 'Create' computation. Uninitialised elements are
-- left uninitialised.
create :: (Storable e, Dim i) => (forall s. Create i e (ST s) ()) -> Array i e
create a = runST (freezeST a)

{-# INLINE freezeIO #-}
freezeIO :: (Storable e, Dim i) => Mode i -> Create i e IO () -> IO (Array i e)
freezeIO m (Create c) = do
  arr <- new Proxy
  runReaderT c arr
  freeze m arr

{-# INLINE mset #-}
mset
  :: (PrimMonad m, Applicative m, Storable e)
  => Mode dim -> e -> Create dim e m ()
mset m a = do
  arr <- ask
  lift (set m arr a)

{-# INLINE mload #-}
mload :: (Rank n dim, PrimMonad m, Applicative m, Storable e)
      => Mode n -> Array n e -> Create dim e m ()
mload m arr = mloadM m (unsafeThaw arr)

{-# INLINE mloadAll #-}
mloadAll
  :: (Rank dim dim, PrimMonad m, Applicative m, Storable e)
  => Mode dim
  -> Array dim e
  -> Create dim e m ()
mloadAll m arr = mloadM m (unsafeThaw arr)

{-# INLINE mloadAllM #-}
mloadAllM
  :: (Rank dim dim, PrimMonad m, Applicative m, Storable e)
  => Mode dim
  -> MArray (PrimState m) dim e
  -> Create dim e m ()
mloadAllM m arr = mloadM m arr

{-# INLINE mloadM #-}
mloadM
  :: (Rank n dim, PrimMonad m, Applicative m, Storable e)
  => Mode n -> MArray (PrimState m) n e -> Create dim e m ()
mloadM m arr = do
  res <- ask
  lift $ withRangeIndices m $ \ix -> do
    e <- unsafeIndex arr ix
    unsafeWrite res ix e

{-# INLINE mloadList #-}
mloadList
  :: forall dim e m. (Applicative m, PrimMonad m, Storable e)
  => [e] -> Create dim e m ()
mloadList list = do
  arr <- ask
  let
    {-# INLINE go #-}
    go :: Int -> [e] -> m ()
    go !ix (x:xs) = unsafeWrite arr ix x *> go (ix+1) xs
    go _   []     = pure ()
  lift (go 0 list)

{-# INLINE (<~) #-}
-- | Write to an index.
(<~) :: (PrimMonad m, Dim dim, Storable e) => dim -> e -> Create dim e m ()
(<~) ix e = toIndex ix !<~ e

infixr 4 <~
infixr 4 !<~

{-# INLINE (!<~) #-}
-- | Unsafe write to an index. Bounds are not checked.
(!<~) :: (PrimMonad m, Storable e) => Int -> e -> Create dim e m ()
(!<~) ix e = do
  arr <- ask
  lift (unsafeWrite arr ix e)

--------------------------------------------------------------------------------
-- Hints

msize :: Applicative m => Proxy dim -> Create dim e m ()
msize _ = pure ()

mof :: Applicative m => a -> Create dim e m ()
mof _ = pure ()

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
