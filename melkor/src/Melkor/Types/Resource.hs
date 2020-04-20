{-|
Module      : Melkor.Types.Resource
Description : Resource data type declaration
Copyright   : (c) Boris Buliga, 2020
License     : MIT
Maintainer  : boris@d12frosted.io
Stability   : experimental
Portability : POSIX
-}

--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Melkor.Types.Resource
  ( Resource
  , GitHost(..)
  , GitOwner
  , GitRepoName
  , GitBranch
  , gitHubRepo
  , gitHubRepo'
  ) where

--------------------------------------------------------------------------------

import           Melkor.Types.Internal.ToString

--------------------------------------------------------------------------------

import           RIO

--------------------------------------------------------------------------------

-- | 'Resource' data type declares something that can be present in the
-- environment. One can install or update it, or even depend on it.
data Resource
  = GitRepo GitHost GitOwner GitRepoName GitBranch
  deriving (Generic, Eq, Ord)

data GitHost = GitHub | GitLab deriving (Generic, Eq, Ord, Enum, Show)
newtype GitOwner = GitOwner Text deriving (Hashable, Eq, Ord, Show, IsString)
newtype GitRepoName = GitRepoName Text deriving (Hashable, Eq, Ord, Show, IsString)
newtype GitBranch = GitBranch Text deriving (Hashable, Eq, Ord, Show, IsString)

instance Hashable GitHost
instance Hashable Resource

--------------------------------------------------------------------------------

gitHubRepo :: GitOwner -> GitRepoName -> Resource
gitHubRepo owner name = gitHubRepo' owner name "master"

gitHubRepo' :: GitOwner -> GitRepoName -> GitBranch -> Resource
gitHubRepo' = GitRepo GitHub

--------------------------------------------------------------------------------

instance Show Resource where
  show = toString . textDisplay

instance Display Resource where
  display (GitRepo host owner repo branch) = mconcat [ display host
                                                     , ":"
                                                     , display owner
                                                     , "/"
                                                     , display repo
                                                     , ":"
                                                     , display branch
                                                     ]
  {-# INLINE display #-}

instance Display GitHost where
  display = displayShow
  {-# INLINE display #-}

instance Display GitOwner where
  textDisplay (GitOwner owner) = owner
  {-# INLINE textDisplay #-}

instance Display GitRepoName where
  textDisplay (GitRepoName repo) = repo
  {-# INLINE textDisplay #-}

instance Display GitBranch where
  textDisplay (GitBranch branch) = branch
  {-# INLINE textDisplay #-}

--------------------------------------------------------------------------------
