-- | This module defines all kind of providers.

--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------------------

module Melkor.Provider where

--------------------------------------------------------------------------------

import           Melkor.Dependency

--------------------------------------------------------------------------------

import           RIO

--------------------------------------------------------------------------------

data Provider
  = Provider
  { providerSatisfies :: Dependency -> Bool
  , providerInstall   :: Dependency -> ()
  }

--------------------------------------------------------------------------------

class HasProviders env where
  providersL :: Lens' env (TVar [Provider])

addProvider :: ( MonadIO m, MonadReader r m, HasProviders r )
            => Provider
            -> m ()
addProvider provider = do
  providers <- view providersL
  atomically $ modifyTVar providers (provider :)

--------------------------------------------------------------------------------

repoProvider :: Provider
repoProvider = Provider { providerSatisfies = \case
                            DRepo {} -> True
                            _        -> False
                        , providerInstall = const ()
                        }

linkProvider :: Provider
linkProvider = Provider { providerSatisfies = \case
                            DLink {} -> True
                            _        -> False
                        , providerInstall = const ()
                        }

--------------------------------------------------------------------------------
