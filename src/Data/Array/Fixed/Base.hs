{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NullaryTypeClasses  #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Data.Array.Fixed.Base where
import           Control.Applicative
import           Control.Monad
import           Data.Index          hiding (size)
import qualified Data.Index          as Index
import           Foreign.ForeignPtr

-- | An @Array i a@ is an array with indices bounded by @i@.
newtype MArray s i a = MArray (ForeignPtr a)
newtype Array i a = Array (ForeignPtr a)
type role MArray nominal representational representational
type role Array representational representational

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
-- Utility

{-# INLINE forArrayIx #-}
forArrayIx
  :: Applicative f
  => array i a
  -> Mode i
  -> (Int -> f ())
  -> f ()
forArrayIx _ m f = withRangeIndices m f
