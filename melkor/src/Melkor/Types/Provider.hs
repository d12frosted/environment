--------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------------------------------------

-- |
-- Module      : Melkor.Types.Provider
-- Description : Provider data type declaration
-- Copyright   : (c) Boris Buliga, 2020
-- License     : MIT
-- Maintainer  : boris@d12frosted.io
-- Stability   : experimental
-- Portability : POSIX
module Melkor.Types.Provider
  ( ProviderName,
    Provider,
    mkProvider,
    providerName,
    providerDependencies,
    providerSatisfies,
    Status (..),
    Eff (..),
  )
where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

import Data.Hashable
import Melkor.Extra.Display
import Melkor.Types.Eff
import Melkor.Types.Internal.ToString
import Melkor.Types.Resource
import RIO

--------------------------------------------------------------------------------

newtype ProviderName
  = ProviderName Text
  deriving (Eq, Ord, Hashable)

instance Display ProviderName where
  textDisplay (ProviderName name) = name
  {-# INLINE textDisplay #-}

instance Show ProviderName where
  show = toString . textDisplay
  {-# INLINE show #-}

--------------------------------------------------------------------------------

-- | 'ProviderM' defines something that can install or update a 'Resource'. It
-- can also depend on other 'Resource' for it to function.
data ProviderM m = ProviderM
  { _name :: ProviderName,
    _dependencies :: HashSet Resource,
    _satisfies :: Resource -> m Bool,
    _status :: Resource -> m Status,
    _install :: [Resource] -> m (),
    _update :: [Resource] -> m ()
  }

instance Hashable (ProviderM m) where
  hashWithSalt s = hashWithSalt s . _name

instance Eq (ProviderM m) where
  p1 == p2 = _name p1 == _name p2

instance Ord (ProviderM m) where
  compare p1 p2 = compare (_name p1) (_name p2)

instance Display (ProviderM m) where
  display ProviderM {..} =
    mconcat
      [ "<",
        display _name,
        ">",
        display $ toList _dependencies
      ]
  {-# INLINE display #-}

instance Show (ProviderM m) where
  show = toString . textDisplay
  {-# INLINE show #-}

--------------------------------------------------------------------------------

newtype Provider
  = Provider (ProviderM Eff)
  deriving (Hashable, Eq, Ord, Display, Show)

providerName :: Provider -> ProviderName
providerName (Provider p) = _name p

providerDependencies :: Provider -> HashSet Resource
providerDependencies (Provider p) = _dependencies p

providerSatisfies :: Provider -> (Resource -> Eff Bool)
providerSatisfies (Provider p) = _satisfies p

mkProvider ::
  Text ->
  HashSet Resource ->
  (Resource -> Eff Bool) ->
  (Resource -> Eff Status) ->
  ([Resource] -> Eff ()) ->
  ([Resource] -> Eff ()) ->
  Provider
mkProvider name deps satisfies status install update =
  Provider $
    ProviderM
      { _name = ProviderName name,
        _dependencies = deps,
        _satisfies = satisfies,
        _status = status,
        _install = install,
        _update = update
      }

--------------------------------------------------------------------------------

data Status
  = Missing
  | Outdated
  | Latest
  | Unknown
  deriving (Enum)

--------------------------------------------------------------------------------
