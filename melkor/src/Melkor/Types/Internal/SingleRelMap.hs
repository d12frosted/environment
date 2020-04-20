{-|
Module      : Melkor.Types.Internal.SingleRelMap
Description : 'SingleRelMap' data declaration
Copyright   : (c) Boris Buliga, 2020
License     : MIT
Maintainer  : boris@d12frosted.io
Stability   : experimental
Portability : POSIX
-}

--------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------------------------------------

module Melkor.Types.Internal.SingleRelMap
  ( SingleRelMap
  , mkSingleRelMap

  , rel
  , ambiguous
  , orphans
  , keys
  , elems

  , member

  ) where

--------------------------------------------------------------------------------

import           Melkor.Extra.Display
import           Melkor.Extra.List
import           Melkor.Types.Internal.ToString

--------------------------------------------------------------------------------

import           RIO
import qualified RIO.HashMap                    as HM
import qualified RIO.HashSet                    as HS
import qualified RIO.List                       as L

--------------------------------------------------------------------------------

data SingleRelMap a b
  = SingleRelMap
  { _rel       :: HashMap a b
  , _ambiguous :: HashMap a (HashSet b)
  , _orphans   :: HashSet a
  } deriving ( Eq )

--------------------------------------------------------------------------------

mkSingleRelMap :: ( Eq a, Hashable a ) => HashMap a (HashSet b) -> SingleRelMap a b
mkSingleRelMap raw
  = SingleRelMap
  { _rel       = HM.mapMaybe (toSingleton . toList) raw
  , _ambiguous = HM.filter (\v -> length v > 1) raw
  , _orphans   = HS.fromList . HM.keys $ HM.filter null raw
  }

--------------------------------------------------------------------------------

rel :: SingleRelMap a b -> HashMap a b
rel SingleRelMap {..} = _rel
{-# INLINE rel #-}

ambiguous :: SingleRelMap a b -> HashMap a (HashSet b)
ambiguous SingleRelMap {..} = _ambiguous
{-# INLINE ambiguous #-}

orphans :: SingleRelMap a b -> HashSet a
orphans SingleRelMap {..} = _orphans
{-# INLINE orphans #-}

keys :: ( Eq a, Hashable a ) => SingleRelMap a b -> HashSet a
keys srm = HS.unions [ HS.fromList . HM.keys . rel $ srm
                     , HS.fromList . HM.keys . ambiguous $ srm
                     , orphans srm
                     ]
{-# INLINE keys #-}

elems :: ( Eq b, Hashable b ) => SingleRelMap a b -> HashSet b
elems srm = HS.unions $
  HS.fromList (HM.elems (rel srm)) : HM.elems (ambiguous srm)
{-# INLINE elems #-}

--------------------------------------------------------------------------------

member :: ( Eq a, Hashable a
          , Eq b, Hashable b
          )
       => a
       -> SingleRelMap a b
       -> Bool
member a srm = HM.member a (rel srm)
  || HM.member a (ambiguous srm)
  || HS.member a (orphans srm)

--------------------------------------------------------------------------------

uncook :: ( Eq a, Hashable a
          , Eq b, Hashable b
          )
       => SingleRelMap a b
       -> HashMap a (HashSet b)
uncook srm = foldr (\res -> HM.insert res (providers res)) HM.empty (keys srm)
  where providers res =
          HS.unions [ HS.fromList $ maybeToList (HM.lookup res (rel srm))
                    , HM.lookupDefault HS.empty res (ambiguous srm)]

--------------------------------------------------------------------------------

instance ( Eq a, Hashable a
         , Eq b, Hashable b
         ) => Semigroup (SingleRelMap a b) where
  m1 <> m2 = mkSingleRelMap raw
    where raw1 = uncook m1
          raw2 = uncook m2
          raw = HM.unionWith HS.union raw1 raw2

instance ( Eq a, Hashable a
         , Eq b, Hashable b
         ) => Monoid (SingleRelMap a b) where
  mempty
    = SingleRelMap
    { _rel       = HM.empty
    , _ambiguous = HM.empty
    , _orphans   = HS.empty
    }

--------------------------------------------------------------------------------

instance (Display a, Display b) => Display (SingleRelMap a b) where
  display SingleRelMap {..}
    = mconcat
    [ "{ "
    , display $ HM.toList _rel
    , " ambiguous ", display . HM.toList . HM.map toList $ _ambiguous
    , " orphans ", display $ HS.toList _orphans
    , " }"
    ]

instance (Display a, Display b) => Show (SingleRelMap a b) where
  show = toString . textDisplay
  {-# INLINE show #-}

--------------------------------------------------------------------------------
