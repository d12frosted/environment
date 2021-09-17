-- | Keybindings for XMonad. Because why not?
module XMonad.Keybindings where

import XMonad
import XMonad.Commands

keybindings :: [(String, X ())]
keybindings =
  [ ("M-q", spawn "eru xmonad"),
    ("M-p", spawn "rofi -show run"),
    ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-mute @DEFAULT_SINK@ false ; pactl set-sink-volume @DEFAULT_SINK@ +5%"),
    ("<XF86AudioLowerVolume>", spawn "pactl set-sink-mute @DEFAULT_SINK@ false ; pactl set-sink-volume @DEFAULT_SINK@ -5%"),
    ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
    ("<XF86AudioMicMute>", spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle"),
    ("<XF86MonBrightnessUp>", spawn "brightness inc"),
    ("<XF86MonBrightnessDown>", spawn "brightness dec"),
    ("M-S-z", spawn "xlock-wrapper"),
    ("M-<Esc>", spawn "switch_kbd_layout --use-cyrillic"),
    ("M-S-<Esc>", spawn "switch_kbd_layout"),
    ("M-<Print>", spawn "scrot"),
    ("M-C-<Print>", spawn "flameshot gui"),
    ("M-o e", spawn "emacs"),
    ("M-o i", firefox)
  ]
