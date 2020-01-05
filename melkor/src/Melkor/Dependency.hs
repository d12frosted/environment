-- | This module defines all kinds of dependencies.

--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------------------

module Melkor.Dependency where

--------------------------------------------------------------------------------

import           RIO

--------------------------------------------------------------------------------

data Dependency
  = DRepo RepoLoc RepoBranch FilePath
  | DLink FilePath FilePath
  deriving Show

instance Display Dependency where
  display (DRepo loc _ fp) = mconcat [ "Repo "
                                     , display loc
                                     , " -> "
                                     , fromString fp
                                     ]
  display (DLink s t) = mconcat [ "Link "
                                , fromString s
                                , " -> "
                                , fromString t
                                ]

--------------------------------------------------------------------------------

class HasDeps env where
  depsL :: Lens' env (TVar [Dependency])

addDep :: ( MonadIO m, MonadReader r m, HasDeps r )
       => Dependency
       -> m ()
addDep dep = do
  deps <- view depsL
  atomically $ modifyTVar deps (dep :)

ghRepo :: ( MonadIO m, MonadReader r m, HasDeps r )
       => GitHubOwner
       -> GitHubRepo
       -> FilePath
       -> m ()
ghRepo owner repo path
  = addDep
  $ DRepo (GitHub GitHubSSH owner repo) (RepoBranch "master") path

--------------------------------------------------------------------------------

data GitHubProtocol = GitHubSSH | GitHubHTTP deriving Show

instance Display GitHubProtocol where
  display GitHubSSH  = "ssh"
  display GitHubHTTP = "https"

data RepoLoc = GitHub GitHubProtocol GitHubOwner GitHubRepo deriving Show

instance Display RepoLoc where
  display (GitHub _ owner repo) = display owner <> "/" <> display repo

newtype GitHubOwner = GitHubOwner Text deriving (Show, IsString, Display)
newtype GitHubRepo  = GitHubRepo Text deriving (Show, IsString, Display)
newtype RepoBranch = RepoBranch Text deriving (Show, IsString, Display)

--------------------------------------------------------------------------------
