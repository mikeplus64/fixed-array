{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.Array.Fixed.Mutable.Update where
import Control.Applicative
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.Trans
import Data.Array.Fixed
import Data.Array.Fixed.Base
import Data.Array.Fixed.Mutable
import Data.Index               hiding (size)
import Foreign                  hiding (new)

newtype Update dim e m a = Update (ReaderT (MArray (PrimState m) dim e) m a)
  deriving (Monad, Functor, Applicative)

deriving instance
  (PrimMonad m, s ~ PrimState m) => MonadReader (MArray s n e) (Update n e m)

deriving instance MonadIO (Update dim e IO)
instance MonadTrans (Update dim e) where lift = Update . lift

{-# INLINE freezeST #-}
freezeST :: (Storable e, Dim i) => Update i e (ST s) () -> ST s (Array i e)
freezeST (Update c) = do
  arr <- new Proxy
  runReaderT c arr
  return (unsafeFreeze arr)

{-# INLINE create #-}
-- | Update an array from a 'Update' computation. Uninitialised elements are
-- left uninitialised.
create :: (Storable e, Dim i) => (forall s. Update i e (ST s) ()) -> Array i e
create a = runST (freezeST a)

{-# INLINE freezeIO #-}
freezeIO :: (Storable e, Dim i) => Mode i -> Update i e IO () -> IO (Array i e)
freezeIO m (Update c) = do
  arr <- new Proxy
  runReaderT c arr
  freeze m arr

{-# INLINE mset #-}
mset
  :: (PrimMonad m, Applicative m, Storable e)
  => Mode dim -> e -> Update dim e m ()
mset m a = do
  arr <- ask
  lift (set m arr a)

{-# INLINE load #-}
load :: (Rank n dim, PrimMonad m, Applicative m, Storable e)
      => Mode n -> Array n e -> Update dim e m ()
load m arr = loadMut m (unsafeThaw arr)

{-# INLINE mzip #-}
mzip
  :: (Storable a, Storable e, Applicative m, PrimMonad m)
  => Mode n -> (a -> e -> e) -> Array n a -> Update n e m ()
mzip m f arr = do
  env <- ask
  lift . withRangeIndices m $ \ix -> do
    e <- unsafeIndex env ix
    unsafeWrite env ix $! f (arr!>ix) e

{-# INLINE mzipMut #-}
mzipMut
  :: (Storable a, Storable e, Applicative m, PrimMonad m)
  => Mode n -> (a -> e -> e) -> MArray (PrimState m) n a -> Update n e m ()
mzipMut m f arr = do
  env <- ask
  lift . withRangeIndices m $ \ix -> do
    a <- unsafeIndex arr ix
    e <- unsafeIndex env ix
    unsafeWrite env ix $! f a e

{-# INLINE mmap #-}
mmap :: (PrimMonad m, Applicative m, Storable e)
     => Mode n -> (e -> e) -> Update n e m ()
mmap m f = do
  env <- ask
  lift . withRangeIndices m $ \ix -> do
    a <- unsafeIndex env ix
    unsafeWrite env ix $! f a

{-# INLINE mfoldr #-}
mfoldr
  :: (Functor m, PrimMonad m, Storable e)
  => Mode dim -> (e -> a -> a) -> a -> Update dim e m a
mfoldr mode f z = do
  arr <- fmap unsafeFreeze ask
  return $! foldrRangeIndices mode
    (\ix acc -> f (arr!>ix) acc)
    z

{-# INLINE mfoldl #-}
mfoldl
  :: (Functor m, PrimMonad m, Storable e)
  => Mode dim -> (a -> e -> a) -> a -> Update dim e m a
mfoldl mode f z = do
  arr <- fmap unsafeFreeze ask
  return $! foldlRangeIndices mode
    (\acc ix -> f acc (arr!>ix))
    z

{-# INLINE loadMut #-}
loadMut
  :: (Rank n dim, PrimMonad m, Applicative m, Storable e)
  => Mode n -> MArray (PrimState m) n e -> Update dim e m ()
loadMut m arr = do
  res <- ask
  lift $ withRangeIndices m $ \ix -> do
    e <- unsafeIndex arr ix
    unsafeWrite res ix e

{-# INLINE loadList #-}
loadList
  :: forall dim e m. (Applicative m, PrimMonad m, Storable e)
  => [e] -> Update dim e m ()
loadList list = do
  arr <- ask
  lift $! do
    let
      go !ix (x:xs) = unsafeWrite arr ix x *> go (ix+1) xs
      go _   []     = pure ()
    go 0 list

{-# INLINE (<~) #-}
-- | Write to an index.
(<~) :: (PrimMonad m, Dim dim, Storable e) => dim -> e -> Update dim e m ()
(<~) ix e = toIndex ix !<~ e

infixr 4 <~
infixr 4 !<~

{-# INLINE (!<~) #-}
-- | Unsafe write to an index. Bounds are not checked.
(!<~) :: (PrimMonad m, Storable e) => Int -> e -> Update dim e m ()
(!<~) ix e = do
  arr <- ask
  lift (unsafeWrite arr ix e)

--------------------------------------------------------------------------------
-- Hints

msize :: Applicative m => Proxy dim -> Update dim e m ()
msize _ = pure ()

mof :: Applicative m => a -> Update dim e m ()
mof _ = pure ()

