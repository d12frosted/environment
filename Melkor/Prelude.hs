-- | Prelude for all Melkor build rules.

--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Melkor.Prelude
  ( -- Reexports
    module RIO
  , module Development.Shake
  , module Development.Shake.FilePath
  , envMaybe, getEnv

  , AppEnv
  , HasAppEnv(..)
  , homeL
  , configHomeL
  , binHomeL
  , sysL

  , RulesT
  , ActionT

  , melkor
  , phonyT

  , putQuietT
  , putLoudT
  , putNormalT

  , wantd
  , needp
  ) where

--------------------------------------------------------------------------------

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Functor
import           Data.Maybe                 (fromMaybe)
import           Development.Shake          hiding (getEnv, (*>))
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Env                        (envMaybe, getEnv)
import           Prelude
import           RIO                        hiding (withTempFile)
import           System.Directory           (createDirectoryIfMissing)

--------------------------------------------------------------------------------

data AppEnv
  = AppEnv
  { _home       :: FilePath
  , _configHome :: FilePath
  , _binHome    :: FilePath
  , _sys        :: OS
  }

class HasAppEnv env where
  appEnvL :: Lens' env AppEnv

instance HasAppEnv AppEnv where
  appEnvL = id

homeL :: Lens' AppEnv FilePath
homeL = lens _home (\x y -> x { _home = y })

configHomeL :: Lens' AppEnv FilePath
configHomeL = lens _configHome (\x y -> x { _configHome = y })

binHomeL :: Lens' AppEnv FilePath
binHomeL = lens _binHome (\x y -> x { _binHome = y })

sysL :: Lens' AppEnv OS
sysL = lens _sys (\x y -> x { _sys = y })

--------------------------------------------------------------------------------

data OS = MacOS | ArchLinux

--------------------------------------------------------------------------------

type RulesT a = ReaderT AppEnv Rules a
type ActionT a = ReaderT AppEnv Action a

melkor :: ShakeOptions -> RulesT () -> IO ()
melkor opts rules = do
  home <- getEnv "HOME"
  configHome <- fromMaybe (home </> ".config") <$> envMaybe "XDG_CONFIG_HOME"
  let appEnv = AppEnv home configHome "/usr/bin" ArchLinux
  shakeArgs opts $ do
    buildDirRule
    runReaderT rules appEnv

--------------------------------------------------------------------------------

phonyT :: String -> ActionT () -> RulesT ()
phonyT name action = do
  env <- view appEnvL
  lift $ phony name (runReaderT action env)

--------------------------------------------------------------------------------

putQuietT :: String -> ActionT ()
putQuietT = lift . putQuiet

putLoudT :: String -> ActionT ()
putLoudT = lift . putLoud

putNormalT :: String -> ActionT ()
putNormalT = lift . putNormal

--------------------------------------------------------------------------------

needp :: [String] -> ReaderT AppEnv Action ()
needp ps = do
  binHome <- view binHomeL
  lift $ need $ (binHome </>) <$> ps

--------------------------------------------------------------------------------

wantd :: MonadTrans t => [FilePath] -> t Rules ()
wantd = lift . want . fmap (</> dirMarker)

buildDirRule :: Rules ()
buildDirRule = ("//*" <> dirMarker) %> \out -> do
  let dir = takeDirectory out
  putQuiet $ "Creating missing directory: " <> dir
  liftIO $ createDirectoryIfMissing True dir
  writeFile' (dir </> dirMarker) ""

dirMarker :: FilePath
dirMarker = ".melkor.dir"

--------------------------------------------------------------------------------
