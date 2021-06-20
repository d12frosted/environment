-- | Keybindings for XMonad. Because why not?
module XMonad.Keybindings where

import XMonad
import XMonad.Commands

keybindings :: [(String, X ())]
keybindings =
  [ ("M-q", spawn "eru xmonad"),
    ("M-p", spawn "rofi -show run"),
    ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+"),
    ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-"),
    ("<XF86AudioMute>", spawn "amixer -D pulse set Master 1+ toggle"),
    ("<XF86AudioMicMute>", spawn "amixer -D pulse set Capture 1+ toggle"),
    ("<XF86MonBrightnessUp>", spawn "brightness inc"),
    ("<XF86MonBrightnessDown>", spawn "brightness dec"),
    ("M-S-z", spawn "xlocker"),
    ("M-<Esc>", spawn "switch_kbd_layout --use-cyrillic"),
    ("M-S-<Esc>", spawn "switch_kbd_layout"),
    ("M-<Print>", spawn "scrot"),
    ("M-C-<Print>", spawn "flameshot gui"),
    ("M-o e", spawn "emacs"),
    ("M-o i", firefox),
    ("M-t n", spawn "notify toggle")
  ]
