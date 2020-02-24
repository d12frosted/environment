{-|
Module      : Melkor.Types.Internal.SingleRelMapSpec
Description : 'SingleRelMap' tests
Copyright   : (c) Boris Buliga, 2020
License     : MIT
Maintainer  : boris@d12frosted.io
Stability   : experimental
Portability : POSIX
-}

--------------------------------------------------------------------------------

module Melkor.Types.Internal.SingleRelMapSpec where

--------------------------------------------------------------------------------

import           Melkor.Test.Gen
import           Melkor.Types.Internal.SingleRelMap

--------------------------------------------------------------------------------

import           RIO
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.QuickCheck.Laws.Monoid

--------------------------------------------------------------------------------

tests :: TestTree
tests
  = testGroup "SingleRelMap"
  [ testMonoidLaws (Proxy :: Proxy (SingleRelMap Bool Word))

  , testProperty "mappend combines keys" $
    prop_mappendKeys (Proxy :: Proxy Bool) (Proxy :: Proxy Word)

  , testProperty "mappend combines elems" $
    prop_mappendKeys (Proxy :: Proxy Bool) (Proxy :: Proxy Word)
  ]

prop_mappendKeys :: ( Show a, Hashable a, Eq a
                    , Show b, Hashable b, Eq b
                    )
                 => Proxy a
                 -> Proxy b
                 -> SingleRelMap a b
                 -> SingleRelMap a b
                 -> Property
prop_mappendKeys _ _ srm1 srm2
  = keys (srm1 <> srm2) === keys srm1 <> keys srm2

prop_mappendElems :: ( Show a, Hashable a, Eq a
                     , Show b, Hashable b, Eq b
                     )
                  => Proxy a
                  -> Proxy b
                  -> SingleRelMap a b
                  -> SingleRelMap a b
                  -> Property
prop_mappendElems _ _ srm1 srm2
  = elems (srm1 <> srm2) === elems srm1 <> elems srm2

--------------------------------------------------------------------------------
