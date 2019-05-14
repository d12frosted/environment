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
  [ ((mod4Mask, xK_q), spawn "eru xmonad")
  , ((mod4Mask, xK_p), spawn "dmenu_run -h 32 -fn 'Source Code Pro-14'")
  , ((0, xF86XK_AudioRaiseVolume), vlmInc)
  , ((0, xF86XK_AudioLowerVolume), vlmDec)
  , ((0, xF86XK_AudioMute), vlmMute)
  , ((0, xF86XK_AudioMicMute), micMute)
  , ((mod4Mask .|. shiftMask, xK_z), spawn "xlocker")
  , ((mod4Mask, xK_Escape), spawn "switch_kbd_layout t")
  , ((mod4Mask .|. shiftMask, xK_Escape), spawn "switch_kbd_layout")
  , ((mod4Mask, xK_Print), spawn "scrot")
  ]
