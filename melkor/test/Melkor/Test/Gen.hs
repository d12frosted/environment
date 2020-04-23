--------------------------------------------------------------------------------

-- |
-- Module      : Melkor.Gen
-- Description : Generators for QuickCheck
-- Copyright   : (c) Boris Buliga, 2020
-- License     : MIT
-- Maintainer  : boris@d12frosted.io
-- Stability   : experimental
-- Portability : POSIX
module Melkor.Test.Gen where

--------------------------------------------------------------------------------

import Melkor.BuildMap
import Melkor.Types.Internal.SingleRelMap
import Melkor.Types.Provider
import Melkor.Types.Resource
--------------------------------------------------------------------------------

import RIO
import qualified RIO.HashMap as HM
import qualified RIO.HashSet as HS
import qualified RIO.List as L
import qualified RIO.Partial as RIO'
import Test.Tasty.QuickCheck
import Prelude (enumFrom)

--------------------------------------------------------------------------------

instance Arbitrary Provider where
  arbitrary =
    mkProvider
      <$> nonEmptyString
      <*> (HS.fromList <$> arbitrary)
      <*> fmap (\b _ -> pure b) arbitrary
      <*> pure (const . pure $ Unknown)
      <*> pure (const . pure $ ())
      <*> pure (const . pure $ ())

providerSupporting :: [Resource] -> Gen Provider
providerSupporting rs =
  mkProvider
    <$> (fromString <$> nonEmptyString)
    <*> (HS.fromList <$> arbitrary)
    <*> pure (\r -> pure $ L.any (== r) rs)
    <*> pure (const . pure $ Unknown)
    <*> pure (const . pure $ ())
    <*> pure (const . pure $ ())

providerSupporting' :: [Resource] -> [Resource] -> Gen Provider
providerSupporting' rs deps =
  mkProvider
    <$> (fromString <$> nonEmptyString)
    <*> pure (HS.fromList deps)
    <*> pure (\r -> pure $ L.any (== r) rs)
    <*> pure (const . pure $ Unknown)
    <*> pure (const . pure $ ())
    <*> pure (const . pure $ ())

providerNotSupporting :: [Resource] -> Gen Provider
providerNotSupporting rs =
  mkProvider
    <$> (fromString <$> nonEmptyString)
    <*> (HS.fromList <$> arbitrary)
    <*> pure (\r -> pure $ not (L.any (== r) rs))
    <*> pure (const . pure $ Unknown)
    <*> pure (const . pure $ ())
    <*> pure (const . pure $ ())

--------------------------------------------------------------------------------

instance Arbitrary Resource where
  arbitrary =
    oneof
      [ gitHubRepo' <$> arbitrary <*> arbitrary <*> arbitrary
      ]

instance Arbitrary GitHost where
  arbitrary = elements $ enumFrom (RIO'.toEnum 0)

instance Arbitrary GitOwner where
  arbitrary = nonEmptyString

instance Arbitrary GitRepoName where
  arbitrary = nonEmptyString

instance Arbitrary GitBranch where
  arbitrary = nonEmptyString

instance Arbitrary Status where
  arbitrary = elements $ enumFrom (RIO'.toEnum 0)

--------------------------------------------------------------------------------

instance
  ( Eq a,
    Hashable a,
    Arbitrary a,
    Eq b,
    Hashable b,
    Arbitrary b
  ) =>
  Arbitrary (SingleRelMap a b)
  where
  arbitrary = mkSingleRelMap . HM.fromList <$> arbitrary

instance Arbitrary BuildMap where
  arbitrary = mkBuildMap . HM.fromList <$> arbitrary

--------------------------------------------------------------------------------

instance (Hashable a, Eq a, Arbitrary a) => Arbitrary (HS.HashSet a) where
  arbitrary = HS.fromList <$> arbitrary
  shrink hashset = HS.fromList <$> shrink (HS.toList hashset)

instance CoArbitrary a => CoArbitrary (HS.HashSet a) where
  coarbitrary = coarbitrary . HS.toList

instance (Hashable a, Eq a, Function a) => Function (HS.HashSet a) where
  function = functionMap HS.toList HS.fromList

--------------------------------------------------------------------------------

nonEmptyString :: IsString a => Gen a
nonEmptyString = fromString <$> listOf1 arbitraryPrintableChar

--------------------------------------------------------------------------------
