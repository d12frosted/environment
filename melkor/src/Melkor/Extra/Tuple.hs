{-|
Module      : Melkor.Extra.Tuple
Description : Utility functions for tuples
Copyright   : (c) Boris Buliga, 2020
License     : MIT
Maintainer  : boris@d12frosted.io
Stability   : experimental
Portability : POSIX
-}

--------------------------------------------------------------------------------

module Melkor.Extra.Tuple where

--------------------------------------------------------------------------------

import           RIO

--------------------------------------------------------------------------------

mapToFst :: (a -> b) -> a -> (b, a)
mapToFst f a = (f a, a)
{-# INLINE mapToFst #-}

mapToSnd :: (a -> b) -> a -> (a, b)
mapToSnd f a = (a, f a)
{-# INLINE mapToSnd #-}

--------------------------------------------------------------------------------

traverseToFst :: Functor t => (a -> t b) -> a -> t (b, a)
traverseToFst f a = (,a) <$> f a
{-# INLINE traverseToFst #-}

traverseToSnd :: Functor t => (a -> t b) -> a -> t (a, b)
traverseToSnd f a = (a,) <$> f a
{-# INLINE traverseToSnd #-}

--------------------------------------------------------------------------------
