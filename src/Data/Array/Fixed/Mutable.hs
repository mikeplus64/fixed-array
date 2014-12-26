{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NullaryTypeClasses  #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE GADTs, KindSignatures       #-}
module Data.Array.Fixed.Mutable where
import           Control.Exception
import           Control.Monad.Primitive
import           Data.Index         hiding (size)
import qualified Data.Index         as Index
import           Foreign.ForeignPtr
import           Foreign.Storable
import           GHC.ForeignPtr     (mallocPlainForeignPtrBytes)

-- | An @Array i a@ is an array with indices bounded by @i@.
newtype MArray s i a = MArray (ForeignPtr a)
type role MArray nominal representational representational

--------------------------------------------------------------------------------
-- Utility

{-# INLINE size #-}
size :: forall s i a. Dim i => MArray s i a -> Int
size _ = Index.size (Proxy :: Proxy i)

{-# INLINE proxySize #-}
proxySize :: Dim i => MArray s i a -> Proxy i
proxySize _ = Proxy

{-# INLINE withSize #-}
withSize :: MArray s i a -> (Proxy i -> b)  -> b
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
index :: (Dim i, Show i, Storable a) => MArray s i a -> i -> IO a
index marray ix
  | i < s     = unsafeIndex marray i
  | otherwise = outOfBounds "read" ix (withSize marray id)
 where
  i = toIndex ix
  s = size marray

{-# INLINE unsafeIndex #-}
-- | Read directly from an index. Bounds are not checked.
unsafeIndex :: Storable a => MArray s i a -> Int -> IO a
unsafeIndex (MArray fptr) ix = withForeignPtr fptr (\ptr -> peekElemOff ptr ix)

--------------------------------------------------------------------------------
-- Writing

write :: (Dim i, Show i, Storable a) => MArray s i a -> i -> a -> IO ()
write marray ix a
  | i < s     = unsafeWrite marray i a
  | otherwise = outOfBounds "write" ix (withSize marray id)
 where
  i = toIndex ix
  s = size marray

{-# INLINE unsafeWrite #-}
-- | Write directly to an index. Bounds are not checked.
unsafeWrite :: Storable a => MArray s i a -> Int -> a -> IO ()
unsafeWrite (MArray fptr) ix a = withForeignPtr fptr
  (\ptr -> pokeElemOff ptr ix a)

{-# INLINE set #-}
-- | Set every element.
set :: forall s i a. Storable a => Mode i -> MArray s i a -> a -> IO ()
set mode marray a = withRangeIndices mode (\ix -> unsafeWrite marray ix a)

test :: IO ()
test = do
  t <- new (Proxy :: Proxy (4:.4:.Z))
  set Unroll t 'a'
  set Roll t 'b'

--------------------------------------------------------------------------------
-- Creation

new :: forall s i a. (Dim i, Storable a) => Proxy i -> IO (MArray s i a)
new s =
  MArray `fmap`
  mallocPlainForeignPtrBytes (Index.size s * sizeOf (undefined :: a))

--------------------------------------------------------------------------------
-- Helpers

-- | Throw an 'IndexOutOfBounds' 'ArrayException'
outOfBounds :: (Dim m, Show m) => String -> m -> Proxy m -> IO b
outOfBounds mode i s =
  throw $ IndexOutOfBounds $
    "Tried to " ++ mode ++ show i ++ " but maxBound = " ++ show
      (reflect `asProxyTypeOf` s)
