--------------------------------------------------------------------------------

-- |
-- Module      : Melkor.BuildMapSpec
-- Description : Specs for 'buildMap'
-- Copyright   : (c) Boris Buliga, 2020
-- License     : MIT
-- Maintainer  : boris@d12frosted.io
-- Stability   : experimental
-- Portability : POSIX
module Melkor.BuildMapSpec where

--------------------------------------------------------------------------------

import Melkor.BuildMap
  ( BuildMap,
    buildMap,
    buildMapDeep,
  )
import qualified Melkor.BuildMap as BM
import Melkor.Test.Gen
import Melkor.Types.Provider
import Melkor.Types.Resource
--------------------------------------------------------------------------------

import RIO hiding (assert)
import qualified RIO.HashMap as HM
import qualified RIO.HashSet as HS
import Test.QuickCheck.Monadic
  ( assert,
    monadicIO,
    pick,
    pre,
    run,
  )
import Test.Tasty
import Test.Tasty.QuickCheck hiding (assert)
import Test.Tasty.QuickCheck.Laws.Monoid

--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "BuildMap"
    [ testGroup
        "buildMap"
        [ testProperty
            "depending on no resources leads to empty build plan"
            prop_emptyRequirements,
          testProperty
            "unsatisfied resources are marked as orphans"
            prop_orphans,
          testProperty
            "ambiguously satisfied resources are marked as such"
            prop_ambiguous
        ],
      testGroup
        "buildMapDeep"
        [ testProperty
            "only dependencies of used providers are added to the final build plan"
            prop_providerDeps,
          testProperty
            "cyclic dependencies don't affect execution"
            prop_cyclic
        ]
    ]

prop_emptyRequirements :: [Provider] -> Property
prop_emptyRequirements providers = monadicIO $ do
  bm <- run . runSimpleApp $ buildMap HS.empty (HS.fromList providers)
  assert $ isEmpty bm

prop_orphans :: Resource -> [Resource] -> Property
prop_orphans resource resources = monadicIO $ do
  provider <- pick $ providerSupporting [resource]
  providers <- pick . listOf $ providerNotSupporting (resource : resources)
  let pset = HS.fromList providers
  pre $ not (HS.member provider pset)
  bm <-
    run . runSimpleApp $
      buildMap
        (HS.insert resource (HS.fromList resources))
        (HS.insert provider pset)
  assert $ all (`isOrphaned` bm) resources
  assert $ isPlanned resource bm

prop_ambiguous :: Resource -> [Resource] -> Property
prop_ambiguous resource resources = monadicIO $ do
  p1 <- pick $ providerSupporting [resource]
  p2 <- pick $ providerSupporting [resource]
  pre $ not (p1 == p2)
  providers <- pick . listOf $ providerNotSupporting (resource : resources)
  let pset = HS.fromList providers
  pre $ not (HS.member p1 pset)
  pre $ not (HS.member p2 pset)
  bm <-
    run . runSimpleApp $
      buildMap
        (HS.insert resource (HS.fromList resources))
        (HS.insert p1 $ HS.insert p2 pset)
  assert $ isAmbiguous resource bm
  assert $ all (`isOrphaned` bm) resources

--------------------------------------------------------------------------------

prop_providerDeps :: Resource -> Property
prop_providerDeps resource = monadicIO $ do
  provider <- pick $ providerSupporting [resource]
  providers <-
    pick . listOf $
      providerNotSupporting (resource : toList (providerDependencies provider))
  let pset = HS.fromList providers
  pre $ not (HS.member provider pset)
  bm <-
    run . runSimpleApp $
      buildMapDeep
        (HS.singleton resource)
        (HS.insert provider pset)
  assert $ isPlanned resource bm
  assert $ all (\r -> BM.member r bm) $ providerDependencies provider
  assert $ all (`notPlanned` bm) (HS.unions (providerDependencies <$> providers))

prop_cyclic :: Resource -> Resource -> Property
prop_cyclic r1 r2 = monadicIO $ do
  p1 <- pick $ providerSupporting' [r1] [r2]
  p2 <- pick $ providerSupporting' [r2] [r1]
  pre $ not (p1 == p2)
  bm <-
    run . runSimpleApp $
      buildMapDeep (HS.fromList [r1, r2]) (HS.fromList [p1, p2])
  assert $ isPlanned r1 bm
  assert $ isPlanned r2 bm

--------------------------------------------------------------------------------

isEmpty :: BuildMap -> Bool
isEmpty bm =
  null (BM.rules bm)
    && null (BM.orphans bm)
    && null (BM.ambiguous bm)

isPlanned :: Resource -> BuildMap -> Bool
isPlanned res bm =
  HM.member res (BM.rules bm)
    && not (HS.member res (BM.orphans bm))
    && not (HM.member res (BM.ambiguous bm))

notPlanned :: Resource -> BuildMap -> Bool
notPlanned res bm = not (HM.member res (BM.rules bm))

isOrphaned :: Resource -> BuildMap -> Bool
isOrphaned res bm =
  not (HM.member res (BM.rules bm))
    && HS.member res (BM.orphans bm)
    && not (HM.member res (BM.ambiguous bm))

isAmbiguous :: Resource -> BuildMap -> Bool
isAmbiguous res bm =
  not (HM.member res (BM.rules bm))
    && not (HS.member res (BM.orphans bm))
    && HM.member res (BM.ambiguous bm)

--------------------------------------------------------------------------------
