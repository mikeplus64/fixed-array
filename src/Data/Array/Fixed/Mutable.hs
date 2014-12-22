{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NullaryTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Data.Array.Fixed.Mutable where
import           Control.Exception
import           Control.Monad.Primitive (unsafeInlineIO)
import           Data.Index              hiding (size)
import qualified Data.Index              as Index
import           Foreign

-- | An @Array i a@ is an array with indices bounded by @i@.
newtype MArray i a = MArray (ForeignPtr a)

{-# INLINE size #-}
size :: forall i a. Dim i => MArray i a -> Int
size _ = Index.size (Proxy :: Proxy i)

sizeP :: MArray i a -> Proxy i
sizeP _ = Proxy

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
index :: (Dim i, Show i, Storable a) => MArray i a -> i -> IO a
index marray ix
  | i < s     = unsafeIndex marray i
  | otherwise = outOfBounds ix (reflect `asProxyTypeOf` sizeP marray)
 where
  i = toIndex ix
  s = size marray

{-# INLINE unsafeIndex #-}
-- | Read directly from an index. Bounds are not checked.
unsafeIndex :: Storable a => MArray i a -> Int -> IO a
unsafeIndex (MArray fptr) ix = withForeignPtr fptr (\ptr -> peekElemOff ptr ix)

--------------------------------------------------------------------------------
-- Playground

testIndexInlining :: MArray (4:.4:.Z) Int -> IO (Int,Int,Int)
testIndexInlining v = do
  a <- index v zero
  b <- index v (succ zero)
  return (a, b, b + a)

--------------------------------------------------------------------------------
-- Errors

outOfBounds :: Show a => a -> a -> IO b
outOfBounds i s = throw $ IndexOutOfBounds $
  "Tried to read " ++ show i ++ " but maxBound = " ++ show s

