-- | Impatient with the emptiness of the Great Void outside the Timeless Halls,
-- and desiring to create things of his own, Melkor often went forth into the
-- Void in search of the Flame Imperishable. But the Flame was of Iluvatar and
-- resided with him...

--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import           Melkor.Prelude
import qualified Test.Build     as TB

--------------------------------------------------------------------------------

main :: IO ()
main = melkor customOptions $ do
  home <- view homeL
  wantd [ home </> ".local/bin" ]

  TB.rules

  -- phony "repo" $
  --   syncRepo (home </> "environment") (Repo GitLab "d12frosted" "environment") "master"

--------------------------------------------------------------------------------

customOptions :: ShakeOptions
customOptions
  = shakeOptions
  { shakeColor = True
  }

--------------------------------------------------------------------------------

-- data RepoHost = GitHub | GitLab

-- data Protocol = HTTPS | Git

-- data Repo
--   = Repo
--   { repoHost :: RepoHost
--   , repoUser :: String
--   , repoName :: String
--   }

-- repoUrl :: Protocol -> Repo -> String
-- repoUrl protocol r =
--   case protocol of
--     HTTPS -> mconcat [ "https://"
--                      , host $ repoHost r
--                      , "/"
--                      , repoUser r
--                      , "/"
--                      , repoName r
--                      , ".git"
--                      ]
--     Git -> mconcat [ "git@"
--                    , host $ repoHost r
--                    , ":"
--                    , repoUser r
--                    , "/"
--                    , repoName r
--                    , ".git"
--                    ]
--   where
--     host GitLab = "gitlab.com"
--     host GitHub = "github.com"

-- gitRule :: FilePath -> Repo -> String -> Rules ()
-- gitRule path repo branch = (path </> ".git/config") %> \_ -> do
--   protocol <- envMaybe "USE_HTTPS" <&> \case
--       Just "true" -> HTTPS
--       Nothing     -> Git
--   cmd_ ["git", "clone", repoUrl Git repo, path, "-b", branch]

-- cloneRepo :: FilePath -> Repo -> String -> Action ()
-- cloneRepo path repo branch = doesDirectoryExist path >>= \case
--   True -> putQuiet $ path <> " already exists"
--   False -> do
--     protocol <- envMaybe "USE_HTTPS" <&> \case
--       Just "true" -> HTTPS
--       Nothing     -> Git
--     cmd_ ["git", "clone", repoUrl Git repo, path, "-b", branch]

-- syncRepo :: FilePath -> Repo -> String -> Action ()
-- syncRepo path repo branch = cloneRepo path repo branch

--------------------------------------------------------------------------------
