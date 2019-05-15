-- | Keybindings for XMonad. Because why not?

--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------
module XMonad.Keybindings where

--------------------------------------------------------------------------------
import           XMonad.Commands

--------------------------------------------------------------------------------
import           XMonad

--------------------------------------------------------------------------------
keybindings :: [(String, X ())]
keybindings =
  [ ("M-q", rebuild)
  , ("M-p", dmenu)
  , ("<XF86AudioRaiseVolume>", vlmInc)
  , ("<XF86AudioLowerVolume>", vlmDec)
  , ("<XF86AudioMute>", vlmMute)
  , ("<XF86AudioMicMute>", micMute)
  , ("M-S-z", xlock)
  , ("M-<Esc>", toggleKbd)
  , ("M-S-<Esc>", toggleKbd')
  , ("M-<Print>", spawn "scrot")
  ]
