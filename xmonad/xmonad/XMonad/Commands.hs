--------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------

-- | Custom interactive commands that usually run by pressing some magic key
-- bindings.
module XMonad.Commands where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

import XMonad
import XMonad.StackSet
import XMonad.Window
import XMonad.Workspaces

--------------------------------------------------------------------------------

dmenu :: MonadIO m => m ()
dmenu = spawn "dmenu_run -l 4 -nb '#f3fcf3' -nf '#050505' -fn 'Source Code Pro-12'"

--------------------------------------------------------------------------------

clipmenu :: MonadIO m => m ()
clipmenu = spawn "clipmenu -i -h 32 -fn 'Source Code Pro-12'"

--------------------------------------------------------------------------------

emacs :: MonadIO m => m ()
emacs = spawn "emacs"

--------------------------------------------------------------------------------

xlock :: MonadIO m => m ()
xlock = spawn "xlocker"

--------------------------------------------------------------------------------

network :: MonadIO m => m ()
network = do
  spawn "nm-applet"
  spawn "networkmanager_dmenu"

--------------------------------------------------------------------------------

firefox :: X ()
firefox =
  findApp "firefox" >>= \case
    Nothing -> spawn "firefox" >> windows (greedyView wsWeb)
    Just win -> windows . focusWindow $ win

--------------------------------------------------------------------------------

rebuild :: MonadIO m => m ()
rebuild = spawn "eru xmonad"

--------------------------------------------------------------------------------

toggleKbd :: MonadIO m => m ()
toggleKbd = spawn "switch_kbd_layout --use-cyrillic"

toggleKbd' :: MonadIO m => m ()
toggleKbd' = spawn "switch_kbd_layout"

--------------------------------------------------------------------------------

vlmInc :: MonadIO m => m ()
vlmInc = spawn "amixer set Master 5%+"

vlmDec :: MonadIO m => m ()
vlmDec = spawn "amixer set Master 5%-"

vlmMute :: MonadIO m => m ()
vlmMute = spawn "amixer -D pulse set Master 1+ toggle"

micMute :: MonadIO m => m ()
micMute = spawn "amixer -D pulse set Capture 1+ toggle"

--------------------------------------------------------------------------------

brightnessInc :: MonadIO m => m ()
brightnessInc = spawn "brightness inc"

brightnessDec :: MonadIO m => m ()
brightnessDec = spawn "brightness dec"

--------------------------------------------------------------------------------

notificationToggle :: MonadIO m => m ()
notificationToggle = spawn "notify toggle"

--------------------------------------------------------------------------------
