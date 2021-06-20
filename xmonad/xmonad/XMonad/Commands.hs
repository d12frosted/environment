{-# LANGUAGE LambdaCase #-}

-- | Custom interactive commands that usually run by pressing some magic key
-- bindings.
module XMonad.Commands where

import XMonad
import XMonad.StackSet
import XMonad.Window
import XMonad.Workspaces

firefox :: X ()
firefox =
  findApp "firefox" >>= \case
    Nothing -> spawn "firefox" >> windows (greedyView wsWeb)
    Just win -> windows . focusWindow $ win
