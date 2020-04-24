--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------

-- |
-- Module      : Melkor.BuildMap
-- Description : 'BuildMap' type definition
-- Copyright   : (c) Boris Buliga, 2020
-- License     : MIT
-- Maintainer  : boris@d12frosted.io
-- Stability   : experimental
-- Portability : POSIX
module Melkor.BuildMap
  ( BuildMap,
    mkBuildMap,
    buildMap,
    buildMapDeep,
    rules,
    ambiguous,
    orphans,
    providers,
    member,
  )
where

--------------------------------------------------------------------------------

import Melkor.Extra.Display ()
import Melkor.Extra.Tuple
import Melkor.Types.Eff
import Melkor.Types.Internal.SingleRelMap
  ( SingleRelMap,
    mkSingleRelMap,
  )
import qualified Melkor.Types.Internal.SingleRelMap as SRM
import Melkor.Types.Provider
import Melkor.Types.Resource
import RIO
import qualified RIO.HashMap as HM
import qualified RIO.HashSet as HS

--------------------------------------------------------------------------------

newtype BuildMap
  = BuildMap (SingleRelMap Resource Provider)
  deriving (Eq, Show, Display, Semigroup, Monoid)

mkBuildMap :: HashMap Resource (HashSet Provider) -> BuildMap
mkBuildMap = BuildMap . mkSingleRelMap
{-# INLINE mkBuildMap #-}

buildMapRaw ::
  (HasContext env) =>
  HashSet Resource ->
  HashSet Provider ->
  RIO env (HashMap Resource (HashSet Provider))
buildMapRaw rset pset =
  HM.fromList
    <$> traverse (traverseToSnd (`providersOf` toList pset)) (toList rset)
  where
    providersOf r =
      fmap HS.fromList
        . filterM (\p -> runEff $ providerSatisfies p r)

unBuildMap :: BuildMap -> SingleRelMap Resource Provider
unBuildMap (BuildMap srm) = srm
{-# INLINE unBuildMap #-}

--------------------------------------------------------------------------------

buildMap ::
  (HasContext env) =>
  HashSet Resource ->
  HashSet Provider ->
  RIO env BuildMap
buildMap rset pset = mkBuildMap <$> buildMapRaw rset pset

buildMapDeep ::
  (HasContext env) =>
  HashSet Resource ->
  HashSet Provider ->
  RIO env BuildMap
buildMapDeep = go mempty
  where
    go bm0 rset pset = do
      bm <- mkBuildMap <$> buildMapRaw rset pset
      let s = bm <> bm0
          rsetExtra =
            mconcat
              . fmap providerDependencies
              . HM.elems
              . rules
              $ bm
          isNew r = not . member r $ s
          rsetNew = HS.filter isNew rsetExtra
      -- traceDisplayM (HS.toList rsetExtra)
      if HS.null rsetNew
        then pure s
        else go s rsetNew pset

--------------------------------------------------------------------------------

rules :: BuildMap -> HashMap Resource Provider
rules (BuildMap m) = SRM.rel m
{-# INLINE rules #-}

ambiguous :: BuildMap -> HashMap Resource (HashSet Provider)
ambiguous (BuildMap m) = SRM.ambiguous m
{-# INLINE ambiguous #-}

orphans :: BuildMap -> HashSet Resource
orphans (BuildMap m) = SRM.orphans m
{-# INLINE orphans #-}

providers :: BuildMap -> HashSet Provider
providers = HS.fromList . HM.elems . rules
{-# INLINE providers #-}

--------------------------------------------------------------------------------

member :: Resource -> BuildMap -> Bool
member r = SRM.member r . unBuildMap
{-# INLINE member #-}

--------------------------------------------------------------------------------
