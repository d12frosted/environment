-- | Collection of available providers.

--------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}

--------------------------------------------------------------------------------

module Melkor.Provider
 ( repoProvider
 , linkProvider

 , module Melkor.Provider.Types
 ) where

--------------------------------------------------------------------------------

import           Melkor.Dependency
import           Melkor.Provider.Types

--------------------------------------------------------------------------------

import           RIO
import           RIO.Directory

--------------------------------------------------------------------------------

repoProvider :: Provider
repoProvider
  = Provider
  { providerSatisfies = \case
      DRepo {} -> True
      _        -> False

  , providerCheckStatus = \case
      DRepo _ _ fp -> doesDirectoryExist fp >>= \case
        True -> pure DependencyUnknown
        False -> pure DependencyMissing
      _ -> pure DependencyUnknown

  , providerInstall = pure . const ()
  }

linkProvider :: Provider
linkProvider
  = Provider
  { providerSatisfies = \case
      DLink {} -> True
      _        -> False
  , providerCheckStatus = pure . const DependencyUnknown
  , providerInstall = pure . const ()
  }

--------------------------------------------------------------------------------
