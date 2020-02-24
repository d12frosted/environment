{-|
Module      : Melkor.Extra.List
Description : Utility functions for lists
Copyright   : (c) Boris Buliga, 2020
License     : MIT
Maintainer  : boris@d12frosted.io
Stability   : experimental
Portability : POSIX
-}

--------------------------------------------------------------------------------

module Melkor.Extra.List where

--------------------------------------------------------------------------------

import           RIO

--------------------------------------------------------------------------------

toSingleton :: [a] -> Maybe a
toSingleton [a] = Just a
toSingleton _   = Nothing

--------------------------------------------------------------------------------
