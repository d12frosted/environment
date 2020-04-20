{-|
Module      : Melkor.BuildPlanSpec
Description : Specs for 'buildPlan'
Copyright   : (c) Boris Buliga, 2020
License     : MIT
Maintainer  : boris@d12frosted.io
Stability   : experimental
Portability : POSIX
-}

--------------------------------------------------------------------------------

module Melkor.BuildPlanSpec
  ( tests
  ) where

--------------------------------------------------------------------------------

import           Melkor.BuildPlan
import           Melkor.Test.Gen
import           Melkor.Types.Internal.ToString
import           Melkor.Types.Provider
import           Melkor.Types.Resource

--------------------------------------------------------------------------------

import           Data.Function                  ((&))
import qualified GHC.Show
import           Prelude                        (enumFrom)
import           RIO                            hiding (assert)
import qualified RIO.HashMap                    as HM
import qualified RIO.HashSet                    as HS
import qualified RIO.List                       as L
import qualified RIO.Partial                    as RIO'
import qualified RIO.Set                        as Set
import           Test.QuickCheck.Monadic        (assert, monadicIO, monitor,
                                                 pick, run)
import           Test.Tasty
import           Test.Tasty.QuickCheck          as QC hiding (assert)

--------------------------------------------------------------------------------

tests :: TestTree
tests
  = testGroup "BuildPlan"
  [ QC.testProperty "depending on no resources leads to empty build plan"
    prop_emptyRequirements

  , QC.testProperty "only dependencies of used providers are added to the final build plan"
    prop_providerDeps

  , QC.testProperty "unsatisfied resources are marked as orphans"
    prop_orphans

  , QC.testProperty "ambiguously satisfied resources are marked as such"
    prop_ambiguous

  , QC.testProperty "transitive dependencies are added"
    prop_transitiveDeps
  ]

prop_emptyRequirements :: [Provider] -> Property
prop_emptyRequirements providers = monadicIO $ do
  plan <- run . runSimpleApp $ buildPlan HS.empty (HS.fromList providers)
  assert $ isEmpty plan

prop_providerDeps :: Resource -> Property
prop_providerDeps resource = monadicIO $ do
  provider <- pick $ providerSupporting [resource]
  providers <- pick . listOf $ providerNotSupporting (resource : toList (providerDependencies provider))
  plan <- run . runSimpleApp $ buildPlan
          (HS.singleton resource)
          (HS.insert provider (HS.fromList providers))
  assert $ isPlanned resource plan
  assert $ all (\r -> isPlanned r plan
                      || isOrphaned r plan
                      || isAmbiguous r plan) $ providerDependencies provider
  assert $ all (`notPlanned` plan) (HS.unions (providerDependencies <$> providers))

prop_orphans :: Resource -> [Resource] -> Property
prop_orphans resource resources = monadicIO $ do
  provider <- pick $ providerSupporting [resource]
  providers <- pick . listOf $ providerNotSupporting (resource : resources)
  plan <- run . runSimpleApp $ buildPlan
          (HS.insert resource (HS.fromList resources))
          (HS.insert provider (HS.fromList providers))
  assert $ all (`isOrphaned` plan) resources
  assert $ isPlanned resource plan

prop_ambiguous :: Resource -> [Resource] -> Property
prop_ambiguous resource resources = monadicIO $ do
  p1 <- pick $ providerSupporting [resource]
  p2 <- pick $ providerSupporting [resource]
  providers <- pick . listOf $ providerNotSupporting (resource : resources)
  plan <- run . runSimpleApp $ buildPlan
          (HS.insert resource (HS.fromList resources))
          (HS.insert p1 $ HS.insert p2 (HS.fromList providers))
  assert $ isAmbiguous resource plan
  assert $ all (`isOrphaned` plan) resources

prop_transitiveDeps :: Property
prop_transitiveDeps = monadicIO $ assert False

--------------------------------------------------------------------------------

isEmpty :: BuildPlan -> Bool
isEmpty plan
  =  null (buildPlanMap plan)
  && null (buildPlanOrphans plan)
  && null (buildPlanAmbiguous plan)

isPlanned :: Resource -> BuildPlan -> Bool
isPlanned resource plan
  =  HM.member resource (buildPlanMap plan)
  && not (HS.member resource (buildPlanOrphans plan))
  && not (HM.member resource (buildPlanAmbiguous plan))

notPlanned :: Resource -> BuildPlan -> Bool
notPlanned resource plan
  =  not (HM.member resource (buildPlanMap plan))

isOrphaned :: Resource -> BuildPlan -> Bool
isOrphaned resource plan
  =  not (HM.member resource (buildPlanMap plan))
  && HS.member resource (buildPlanOrphans plan)
  && not (HM.member resource (buildPlanAmbiguous plan))

isAmbiguous :: Resource -> BuildPlan -> Bool
isAmbiguous resource plan
  =  not (HM.member resource (buildPlanMap plan))
  && not (HS.member resource (buildPlanOrphans plan))
  && HM.member resource (buildPlanAmbiguous plan)

--------------------------------------------------------------------------------
