-- | Helpers and utilities for Window search and manipulation.

--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

--------------------------------------------------------------------------------

module XMonad.Window where

--------------------------------------------------------------------------------

import           Control.Applicative      ((<$>))
import           Data.List                (find)
import           Graphics.X11.Xlib.Extras
import           Prelude
import           XMonad
import           XMonad.StackSet

--------------------------------------------------------------------------------

findApp :: String -> X (Maybe Window)
findApp name = withDisplay $ \dis -> do
  wins <- withWindowSet (pure . allWindows)
  wmap <- traverse (classify dis) wins
  return (snd <$> find ((== name) . fst) wmap)
  where classify :: Display -> Window -> X (String, Window)
        classify dis win = fmap ((, win) . resClass) $ io $ getClassHint dis win

switchToApp :: String -> X ()
switchToApp name =
  findApp name >>= flip whenJust (windows . focusWindow)

--------------------------------------------------------------------------------
