{-|
Module      : Melkor.BuildPlan
Description : 'BuildPlan' type definition
Copyright   : (c) Boris Buliga, 2020
License     : MIT
Maintainer  : boris@d12frosted.io
Stability   : experimental
Portability : POSIX
-}

--------------------------------------------------------------------------------

module Melkor.BuildPlan
  ( BuildPlan(..)
  , buildPlan
  ) where

--------------------------------------------------------------------------------

import           Melkor.BuildMap       (BuildMap, buildMap)
import qualified Melkor.BuildMap       as BM
import           Melkor.Types.Eff
import           Melkor.Types.Provider
import           Melkor.Types.Resource

--------------------------------------------------------------------------------

import           RIO
import qualified RIO.HashMap           as HM
import qualified RIO.HashSet           as HS

--------------------------------------------------------------------------------

data BuildPlan
  = BuildPlan
  { buildPlanMap       :: HashMap Resource Provider
  , buildPlanOrphans   :: HashSet Resource
  , buildPlanAmbiguous :: HashMap Resource [Provider]
  }

fromBuildMap :: BuildMap -> BuildPlan
fromBuildMap bm = BuildPlan { buildPlanMap = BM.rules bm
                            , buildPlanOrphans = BM.orphans bm
                            , buildPlanAmbiguous = HM.map toList $ BM.ambiguous bm
                            }

buildPlan :: ( HasContext env )
          => HashSet Resource
          -> HashSet Provider
          -> RIO env BuildPlan
buildPlan rset pset = do
  bm <- buildMap rset pset
  bm' <- buildMap (HS.unions $ providerDependencies <$> toList (BM.providers bm)) pset
  pure $ fromBuildMap (bm <> bm')

--------------------------------------------------------------------------------
