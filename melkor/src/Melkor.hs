{-|
Module      : Melkor
Description : Entry point for Melkor
Copyright   : (c) Boris Buliga, 2020
License     : MIT
Maintainer  : boris@d12frosted.io
Stability   : experimental
Portability : POSIX
-}

--------------------------------------------------------------------------------

module Melkor
  ( -- runMelkor
  -- , addProvider
  -- , addDependency
  ) where

--------------------------------------------------------------------------------

import           Melkor.BuildPlan
import           Melkor.Types.Provider (Provider)
import qualified Melkor.Types.Provider as Provider
import           Melkor.Types.Resource (Resource)
import qualified Melkor.Types.Resource as Resource

--------------------------------------------------------------------------------

import           RIO

--------------------------------------------------------------------------------

-- -- | Run Melkor effect.
-- --
-- --   1. Check that all dependencies can be satisfied unambiguously. E.g.
-- --      \(\forall d \in D \exist! p \in P: p \text{satisfies} d \).
-- runMelkor :: ( Members '[ Embed IO
--                         , Log Text
--                         , Error BuildPlanError
--                         ] r
--              )
--           => Sem (Reader (TVar [Provider]) ': Reader (TVar [Resource]) ': r) ()
--           -> Sem r ()
-- runMelkor melkor = do
--   log "He who arises in might has awaken!"
--   runTVar [] . runTVar [] $ do
--     melkor
--     -- providers <- readProviders

--     -- add all providers' dependencies to global dependencies
--     -- mapM_ addDependency $ concat (Provider.dependencies <$> providers)
--    -- dependencies <- readDependencies

--     -- prepare resource to provider map
--     throw UnknownError

--     -- log $ show dependencies

-- --------------------------------------------------------------------------------

-- addProvider :: Members '[ Reader (TVar [Provider]), Embed IO ] r
--             => Provider
--             -> Sem r ()
-- addProvider = appendTVar

-- readProviders :: ( Member (Reader (TVar [Provider])) r
--                  , Member (Embed IO) r
--                  )
--               => Sem r [Provider]
-- readProviders = askTVar

-- --------------------------------------------------------------------------------

-- addDependency :: Members '[ Reader (TVar [Resource]), Embed IO ] r
--               => Resource
--               -> Sem r ()
-- addDependency = appendTVar

-- readDependencies :: Members '[ Reader (TVar [Resource]), Embed IO ] r
--                  => Sem r [Resource]
-- readDependencies = askTVar

-- --------------------------------------------------------------------------------

-- appendTVar :: Members '[ Reader (TVar [a]), Embed IO ] r
--            => a
--            -> Sem r ()
-- appendTVar x = ask >>= embed . atomically . flip modifyTVar' (x :)

-- askTVar :: Members '[ Reader (TVar a), Embed IO ] r
--         => Sem r a
-- askTVar = ask >>= embed . readTVarIO

-- runTVar :: Member (Embed IO) r
--         => a
--         -> Sem (Reader (TVar a) ': r) b
--         -> Sem r b
-- runTVar x p = do
--   var <- embed $ newTVarIO x
--   runReader var p

--------------------------------------------------------------------------------
