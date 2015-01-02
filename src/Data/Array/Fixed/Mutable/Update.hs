module Data.Array.Fixed.Mutable.Update where
import Data.Array.Fixed.Mutable
import Data.Array.Fixed.Base


--------------------------------------------------------------------------------
-- Monadic creation interface

newtype Update dim e m a = Update (MArray (PrimState m) dim e m a)
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

{-# INLINE mload #-}
mload :: (Rank n dim, PrimMonad m, Applicative m, Storable e)
      => Mode n -> Array n e -> Update dim e m ()
mload m arr = mloadM m (unsafeThaw arr)

{-# INLINE mzipWith #-}
mzipWith :: Mode n -> (a -> e -> m e) -> Array n e -> Update n e m ()
mzipWith m f arr = do
  env <- ask
  lift . withRangeIndices m $ \ix -> do
    a  <- unsafeIndex arr ix
    e  <- unsafeIndex env ix
    e' <- f a e
    unsafeWrite env ix e'

{-# INLINE mloadAll #-}
mloadAll
  :: (Rank dim dim, PrimMonad m, Applicative m, Storable e)
  => Mode dim
  -> Array dim e
  -> Update dim e m ()
mloadAll m arr = mloadM m (unsafeThaw arr)

{-# INLINE mloadAllM #-}
mloadAllM
  :: (Rank dim dim, PrimMonad m, Applicative m, Storable e)
  => Mode dim
  -> MArray (PrimState m) dim e
  -> Update dim e m ()
mloadAllM m arr = mloadM m arr

{-# INLINE mloadM #-}
mloadM
  :: (Rank n dim, PrimMonad m, Applicative m, Storable e)
  => Mode n -> MArray (PrimState m) n e -> Update dim e m ()
mloadM m arr = do
  res <- ask
  lift $ withRangeIndices m $ \ix -> do
    e <- unsafeIndex arr ix
    unsafeWrite res ix e

{-# INLINE mloadList #-}
mloadList
  :: forall dim e m. (Applicative m, PrimMonad m, Storable e)
  => [e] -> Update dim e m ()
mloadList list = do
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

