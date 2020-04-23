--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------

-- |
-- Module      : Melkor.Types.Internal.ToString
-- Description : Type class for things that can be converted to string.
-- Copyright   : (c) Boris Buliga, 2020
-- License     : MIT
-- Maintainer  : boris@d12frosted.io
-- Stability   : experimental
-- Portability : POSIX
module Melkor.Types.Internal.ToString where

--------------------------------------------------------------------------------

import RIO
import qualified RIO.Text as T

--------------------------------------------------------------------------------

class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id
  {-# INLINE toString #-}

instance ToString Text where
  toString = T.unpack
  {-# INLINE toString #-}

--------------------------------------------------------------------------------
