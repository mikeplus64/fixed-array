{-# LANGUAGE RankNTypes #-}
module Data.Array.Fixed
  ( Array
  , size
  , (!), (!>)
  , map
  , foldl
  , foldr
  , modify
  , Mut.thaw, Mut.freeze
  , Mut.unsafeThaw, Mut.unsafeFreeze
  )
  where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Array.Fixed.Base
import qualified Data.Array.Fixed.Mutable as Mut
import           Data.Index               hiding (size)
import           Foreign
import           Prelude                  hiding (foldl, foldr, map)


(!) :: (Dim i, Storable a) => Array i a -> i -> a
(!) arr i = unsafeInlineIO (Mut.index (Mut.unsafeThaw arr) i)

(!>) :: Storable a => Array i a -> Int -> a
(!>) arr i = unsafeInlineIO (Mut.unsafeIndex (Mut.unsafeThaw arr) i)

--------------------------------------------------------------------------------
-- Modification

{-# INLINE modify #-}
modify
  :: (Storable a, Dim i)
  => (forall s. MArray s i a -> ST s ()) -> Array i a -> Array i a
modify f arr = runST $ do
  m@(MArray fptr) <- Mut.copy Roll (Mut.unsafeThaw arr)
  f m
  return (Array fptr)

--------------------------------------------------------------------------------
-- Element-wise operations

{-# INLINE map #-}
-- | Map over an array.
map :: (Storable a, Storable b, Dim i)
    => Mode i -> (a -> b) -> Array i a -> Array i b
map m f arr = runST $ do
  result <- Mut.new (proxySize arr)
  withRangeIndices m $ \ix ->
    Mut.unsafeWrite result ix $! f (arr!>ix)
  return (Mut.unsafeFreeze result)

--------------------------------------------------------------------------------
-- Folding

{-# INLINE foldl #-}
-- | Strict left fold over an array.
foldl :: Storable a => Mode n -> (b -> a -> b) -> b -> Array i a -> b
foldl m f z arr = foldlRangeIndices m (\b ix -> f b (arr!>ix)) z

{-# INLINE foldr #-}
-- | Lazy right fold over an array.
foldr :: Storable a => Mode n -> (a -> b -> b) -> b -> Array i a -> b
foldr m f z arr = foldrRangeIndices m (\ix b -> f (arr!>ix) b) z



