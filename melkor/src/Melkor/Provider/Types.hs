-- | Provider data type definition and everything you need to define a custom
-- provider.

--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------------------

module Melkor.Provider.Types where

--------------------------------------------------------------------------------

import           Melkor.Dependency

--------------------------------------------------------------------------------

import           RIO

--------------------------------------------------------------------------------

data Provider
  = Provider
  { providerSatisfies   :: Dependency -> Bool
  , providerCheckStatus :: Dependency -> Eff DependencyStatus
  , providerInstall     :: Dependency -> Eff ()
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

newtype Ctx
  = Ctx
  { ctxLogFunc :: LogFunc
  }

instance HasLogFunc Ctx where
  logFuncL = lens ctxLogFunc (\x y -> x { ctxLogFunc = y })

--------------------------------------------------------------------------------

newtype Eff a
  = Eff
  { unEff :: Ctx -> RIO Ctx a
  } deriving (Functor)

instance Applicative Eff where
  pure = Eff . const . pure
  (Eff f) <*> (Eff v) = Eff $ \ctx -> f ctx <*> v ctx

instance Monad Eff where
  m >>= k = Eff $ \ctx -> do
    a <- unEff m ctx
    unEff (k a) ctx

instance MonadIO Eff where
  liftIO = Eff . const . liftIO

runEff :: ( HasLogFunc env ) => Eff a -> RIO env a
runEff = runEff' (\env -> Ctx $ env ^. logFuncL)

runEff' :: (env -> Ctx) -> Eff a -> RIO env a
runEff' f (Eff m) = do
  env <- asks f
  runRIO env (m env)


--------------------------------------------------------------------------------
