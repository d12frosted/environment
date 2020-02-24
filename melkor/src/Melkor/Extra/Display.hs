{-|
Module      : Melkor.Extra.Display
Description : Utilities for 'Display' type class
Copyright   : (c) Boris Buliga, 2020
License     : MIT
Maintainer  : boris@d12frosted.io
Stability   : experimental
Portability : POSIX
-}

--------------------------------------------------------------------------------

module Melkor.Extra.Display where

--------------------------------------------------------------------------------

import           RIO
import           RIO.List (intersperse)

--------------------------------------------------------------------------------

instance Display Bool where
  display = displayShow
  {-# INLINE display #-}

--------------------------------------------------------------------------------

instance Display a => Display (Maybe a) where
  display = displayMaybe
  {-# INLINE display #-}

displayMaybe :: Display a => Maybe a -> Utf8Builder
displayMaybe Nothing  = "Nothing"
displayMaybe (Just a) = display a
{-# INLINE displayMaybe #-}

--------------------------------------------------------------------------------

instance Display a => Display [a] where
  display = displayList
  {-# INLINE display #-}

displayList :: Display a => [a] -> Utf8Builder
displayList xs = "[" <> dc xs <> "]"
  where dc   = mconcat . intersperse ", " . fmap display
{-# INLINE displayList #-}

--------------------------------------------------------------------------------

instance (Display a, Display b) => Display (a, b) where
  display = displayTuple
  {-# INLINE display #-}

displayTuple :: (Display a, Display b) => (a, b) -> Utf8Builder
displayTuple (a, b) = mconcat [ "(", display a, ", ", display b, ")" ]
{-# INLINE displayTuple #-}

--------------------------------------------------------------------------------
