-- | Keybindings for XMonad. Because why not?

--------------------------------------------------------------------------------
module XMonad.Keybindings where

--------------------------------------------------------------------------------
import XMonad.Commands

--------------------------------------------------------------------------------
import Graphics.X11.ExtraTypes.XF86
import XMonad

--------------------------------------------------------------------------------
keybindings :: [((KeyMask, KeySym), X ())]
keybindings =
  [ ((mod4Mask, xK_q), rebuild)
  , ((mod4Mask, xK_p), dmenu)
  , ((0, xF86XK_AudioRaiseVolume), vlmInc)
  , ((0, xF86XK_AudioLowerVolume), vlmDec)
  , ((0, xF86XK_AudioMute), vlmMute)
  , ((0, xF86XK_AudioMicMute), micMute)
  , ((mod4Mask .|. shiftMask, xK_z), xlock)
  , ((mod4Mask, xK_Escape), toggleKbd)
  , ((mod4Mask .|. shiftMask, xK_Escape), toggleKbd')
  , ((mod4Mask, xK_Print), spawn "scrot")
  ]
