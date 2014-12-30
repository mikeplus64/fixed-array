{-# LANGUAGE RankNTypes #-}
module Data.Array.Fixed
  ( Array
  , size
  , (!), (!>)
  , foldl
  , foldr
  )
  where
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Array.Fixed.Base
import qualified Data.Array.Fixed.Mutable as Mut
import           Data.Index               hiding (size)
import           Foreign
import           Prelude                  hiding (foldl, foldr)


(!) :: (Dim i, Storable a) => Array i a -> i -> a
(!) arr i = unsafeInlineIO (Mut.index (marr arr) i)

(!>) :: Storable a => Array i a -> Int -> a
(!>) arr i = unsafeInlineIO (Mut.unsafeIndex (marr arr) i)

--------------------------------------------------------------------------------
-- Modification

modify :: (forall s. MArray s i a -> ST s ()) -> Array i a -> Array i a
modify f arr = runST (f =<< Mut.copy (marr arr))

-- don't modify the result of this!
marr :: Array i a -> MArray RealWorld i a
marr (Array fptr) = MArray fptr

--------------------------------------------------------------------------------
-- Mapping

map :: (a -> b) -> Array i a -> Array i b
map = undefined

--------------------------------------------------------------------------------
-- Folding

foldl :: Storable a => Mode n -> (b -> a -> b) -> b -> Array i a -> b
foldl m f z arr = foldlRangeIndices m (\b ix -> f b (arr!>ix)) z

foldr :: Storable a => Mode n -> (a -> b -> b) -> b -> Array i a -> b
foldr m f z arr = foldrRangeIndices m (\ix b -> f (arr!>ix) b) z
