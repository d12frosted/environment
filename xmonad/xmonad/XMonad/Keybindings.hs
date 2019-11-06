-- | Keybindings for XMonad. Because why not?

--------------------------------------------------------------------------------

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
  , ("<XF86MonBrightnessUp>", brightnessInc)
  , ("<XF86MonBrightnessDown>", brightnessDec)
  , ("M-S-z", xlock)
  , ("M-<Esc>", toggleKbd)
  , ("M-S-<Esc>", toggleKbd')
  , ("M-<Print>", spawn "scrot")
  , ("M-o c", clipmenu)
  , ("M-o e", emacs)
  , ("M-o n", network)
  , ("M-o i", firefox)
  , ("M-t n", notificationToggle)
  ]

--------------------------------------------------------------------------------
