-- | Core of the Melkor himself. Gives you the means to run Melkor.

--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

--------------------------------------------------------------------------------

module Melkor.Core
  ( runMelkor
  , Melkor

  , module Melkor.Dependency
  , module Melkor.Provider
  ) where

--------------------------------------------------------------------------------

import           Melkor.Dependency
import           Melkor.Provider

--------------------------------------------------------------------------------

import           RIO
import qualified RIO.List          as L
import qualified RIO.Text          as T

--------------------------------------------------------------------------------

runMelkor :: RIO Melkor () -> IO ()
runMelkor melkor = run $ do
  logInfo "Melkor is here!"

  -- run user defined rules
  melkor

  -- check that all dependencies can be satisfied
  allDeps <- view depsL >>= readTVarIO
  allProviders <- view providersL >>= readTVarIO
  let unsatisfied = filter (\dep -> not (any (`providerSatisfies` dep) allProviders)) allDeps
  if null unsatisfied
  then logInfo "everything is nice, all dependencies can be satisfied"
  else logError $ "Some of the dependencies can't be satisfied\n"
                <> (display . T.intercalate "\n" . map (("  " <>) . textDisplay) $ unsatisfied)
  -- TODO: fail when any of the dependencies can't be satisfied

  let depToProviderMap = map (\dep -> (dep, fromMaybe undefined (L.find (`providerSatisfies` dep) allProviders))) allDeps
  statuses <- mapM (\(dep, provider) -> (dep,) <$> runEff (providerCheckStatus provider dep)) depToProviderMap
  mapM_ (\(dep, status) -> logInfo $ display status <> " :: " <> display dep) statuses

  logInfo "done."

run :: RIO Melkor a -> IO a
run inner = do
  logOptions <- setLogUseTime False .
                setLogUseLoc False .
                setLogUseColor True <$>
                logOptionsHandle stdout True
  deps <- newTVarIO []
  providers <- newTVarIO []
  withLogFunc logOptions $ \logFunc -> do
    let app = Melkor
          { mLogFunc   = logFunc
          , mDeps      = deps
          , mProviders = providers
          }
    runRIO app inner

--------------------------------------------------------------------------------

data Melkor
 = Melkor
 { mLogFunc   :: !LogFunc
 , mDeps      :: !(TVar [Dependency])
 , mProviders :: !(TVar [Provider])
 }

instance HasLogFunc Melkor where
  logFuncL = lens mLogFunc (\x y -> x { mLogFunc = y })

instance HasDeps Melkor where
  depsL = lens mDeps (\x y -> x { mDeps = y })

instance HasProviders Melkor where
  providersL = lens mProviders (\x y -> x { mProviders = y })

--------------------------------------------------------------------------------
