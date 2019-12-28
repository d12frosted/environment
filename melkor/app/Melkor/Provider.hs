-- | This module defines all kind of providers.

--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

--------------------------------------------------------------------------------

module Melkor.Provider where

--------------------------------------------------------------------------------

import           Melkor.Dependency

--------------------------------------------------------------------------------

import           RIO
import           RIO.Directory

--------------------------------------------------------------------------------

data Provider
  = Provider
  { providerSatisfies   :: Dependency -> Bool
  , providerCheckStatus :: Dependency -> Eff DependencyStatus
  , providerInstall     :: Dependency -> Eff ()
  }

data DependencyStatus
  = DependencyMissing
  | DependencyOutdated
  | DependencyLatest
  | DependencyUnknown

instance Display DependencyStatus where
  display DependencyMissing  = "missing"
  display DependencyOutdated = "outdated"
  display DependencyLatest   = "latest"
  display DependencyUnknown  = "unknown"

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
repoProvider
  = Provider
  { providerSatisfies = \case
      DRepo {} -> True
      _        -> False

  , providerCheckStatus = \case
      DRepo loc branch fp -> doesDirectoryExist fp >>= \case
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

newtype Ctx
  = Ctx
  { ctxLogFunc :: LogFunc
  }

instance HasLogFunc Ctx where
  logFuncL = lens ctxLogFunc (\x y -> x { ctxLogFunc = y })

newtype Eff a
  = Eff
  { unEff :: Ctx -> RIO Ctx a
  } deriving (Functor)

instance Applicative Eff where
  pure a = Eff $ \ctx -> pure a
  (Eff f) <*> (Eff v) = Eff $ \ctx -> f ctx <*> v ctx

instance Monad Eff where
  m >>= k = Eff $ \ctx -> do
    a <- unEff m ctx
    unEff (k a) ctx

instance MonadIO Eff where
  liftIO m = Eff $ \ctx -> liftIO m

runEff :: ( HasLogFunc env ) => Eff a -> RIO env a
runEff = runEff' (\env -> Ctx $ env ^. logFuncL)

runEff' :: (env -> Ctx) -> Eff a -> RIO env a
runEff' f (Eff m) = do
  env <- asks f
  runRIO env (m env)

--------------------------------------------------------------------------------
