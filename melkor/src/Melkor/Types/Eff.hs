{-|
Module      : Melkor.Types.Eff
Description : Eff type class declaration
Copyright   : (c) Boris Buliga, 2020
License     : MIT
Maintainer  : boris@d12frosted.io
Stability   : experimental
Portability : POSIX
-}

--------------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds #-}

--------------------------------------------------------------------------------

module Melkor.Types.Eff where

--------------------------------------------------------------------------------

import           RIO

--------------------------------------------------------------------------------

type HasContext m = HasLogFunc m

newtype Context
  = Context
  { ctxLogFunc :: LogFunc
  }

instance HasLogFunc Context where
  logFuncL = lens ctxLogFunc (\x y -> x { ctxLogFunc = y })

--------------------------------------------------------------------------------

newtype Eff a
  = Eff
  { unEff :: Context -> RIO Context a
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

runEff :: ( HasContext env ) => Eff a -> RIO env a
runEff = runEff' (\env -> Context $ env ^. logFuncL)

runEff' :: (env -> Context) -> Eff a -> RIO env a
runEff' f (Eff m) = do
  env <- asks f
  runRIO env (m env)

--------------------------------------------------------------------------------
