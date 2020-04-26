--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

-- |
-- Module      : Melkor
-- Description : Entry point for Melkor
-- Copyright   : (c) Boris Buliga, 2020
-- License     : MIT
-- Maintainer  : boris@d12frosted.io
-- Stability   : experimental
-- Portability : POSIX
module Melkor
  ( runMelkor,
    Melkor,
    depend,
    using,
  )
where

--------------------------------------------------------------------------------

import Melkor.BuildPlan
import Melkor.Extra.Display
import Melkor.Types.Provider (Provider)
import Melkor.Types.Resource (Resource)
import RIO
import qualified RIO.HashMap as HM
import qualified RIO.HashSet as HS
import qualified RIO.List as L

--------------------------------------------------------------------------------

data Melkor = Melkor
  { mLogFunc :: !LogFunc,
    mDeps :: !(TVar (HashSet Resource)),
    mProviders :: !(TVar (HashSet Provider))
  }

instance HasLogFunc Melkor where
  logFuncL = lens mLogFunc (\x y -> x {mLogFunc = y})

instance HasDependencies Melkor where
  depsL = lens mDeps (\x y -> x {mDeps = y})

instance HasProviders Melkor where
  providersL = lens mProviders (\x y -> x {mProviders = y})

--------------------------------------------------------------------------------

data MelkorException
  = OrphanResource Resource
  | AmbiguousProviders Resource [Provider]
  deriving (Typeable)

instance Exception MelkorException

instance Show MelkorException where
  show (OrphanResource res) =
    mconcat
      [ "OrphanResource exception: ",
        stringDisplay res
      ]
  show (AmbiguousProviders res ps) =
    mconcat $
      [ "AmbiguousProviders exception: ",
        stringDisplay res,
        " supported by "
      ]
        <> (L.intersperse ", " . map stringDisplay $ ps)

--------------------------------------------------------------------------------

runMelkor :: RIO Melkor () -> IO ()
runMelkor melkor = run $ do
  logInfo "He who arises in might has awaken!"
  melkor

  deps <- atomically =<< readTVar <$> view depsL
  logDebug "Dependencies:"
  mapM_ (logDebug . (" - " <>) . display) deps

  providers <- atomically =<< readTVar <$> view providersL
  logDebug "Providers:"
  mapM_ (logDebug . (" - " <>) . display) providers

  plan <- buildPlan deps providers
  logDebug "Build plan (orphans):"
  mapM_ (logDebug . (" - " <>) . display) (buildPlanOrphans plan)
  logDebug "Build plan (ambiguous):"
  mapM_ (logDebug . (" - " <>) . displayPair) (HM.toList $ buildPlanAmbiguous plan)
  logDebug "Build plan:"
  mapM_ (logDebug . (" - " <>) . displayPair) (HM.toList $ buildPlanMap plan)

  when (not . HS.null $ buildPlanOrphans plan) $
    mapM_ (throwIO . OrphanResource) (buildPlanOrphans plan)

  when (not . HM.null $ buildPlanAmbiguous plan) $
    mapM_ (throwIO . uncurry AmbiguousProviders) (HM.toList $ buildPlanAmbiguous plan)

  logDebug "prepare build order"
  logDebug "check status of each resource"
  logDebug "install/update if needed"

run :: RIO Melkor a -> IO a
run inner = do
  logOptions <-
    setLogUseTime False
      . setLogUseLoc False
      . setLogUseColor True
      <$> logOptionsHandle stdout True
  deps <- newTVarIO $ HS.empty
  providers <- newTVarIO $ HS.empty
  withLogFunc logOptions $ \logFunc -> do
    let app =
          Melkor
            { mLogFunc = logFunc,
              mDeps = deps,
              mProviders = providers
            }
    runRIO app inner

--------------------------------------------------------------------------------

class HasDependencies env where
  depsL :: Lens' env (TVar (HashSet Resource))

depend :: (HasDependencies env) => Resource -> RIO env ()
depend res = do
  deps <- view depsL
  atomically $ modifyTVar' deps (HS.insert res)

--------------------------------------------------------------------------------

class HasProviders env where
  providersL :: Lens' env (TVar (HashSet Provider))

using :: (HasProviders env) => Provider -> RIO env ()
using provider = do
  providers <- view providersL
  atomically $ modifyTVar' providers (HS.insert provider)

--------------------------------------------------------------------------------
