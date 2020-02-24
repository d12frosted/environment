{-|
Module      : Melkor.Gen
Description : Generators for QuickCheck
Copyright   : (c) Boris Buliga, 2020
License     : MIT
Maintainer  : boris@d12frosted.io
Stability   : experimental
Portability : POSIX
-}

--------------------------------------------------------------------------------

module Melkor.Test.Gen where

--------------------------------------------------------------------------------

import           Melkor.Types.Internal.SingleRelMap

--------------------------------------------------------------------------------

import           RIO
import qualified RIO.HashMap                        as HM
import qualified RIO.HashSet                        as HS
import           Test.Tasty.QuickCheck

--------------------------------------------------------------------------------

instance ( Eq a, Hashable a, Arbitrary a
         , Eq b, Hashable b, Arbitrary b ) => Arbitrary (SingleRelMap a b) where
  arbitrary = mkSingleRelMap . HM.fromList <$> arbitrary

--------------------------------------------------------------------------------

instance (Hashable a, Eq a, Arbitrary a) => Arbitrary (HS.HashSet a) where
  arbitrary = HS.fromList <$> arbitrary
  shrink hashset = HS.fromList <$> shrink (HS.toList hashset)

instance CoArbitrary a => CoArbitrary (HS.HashSet a) where
  coarbitrary = coarbitrary . HS.toList

instance (Hashable a, Eq a, Function a) => Function (HS.HashSet a) where
  function = functionMap HS.toList HS.fromList

--------------------------------------------------------------------------------
