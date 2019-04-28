{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO)
import Data.Default (def)
import Graphics.X11.ExtraTypes.XF86
import System.Taffybar.Support.PagerHints (pagerHints)
import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.EZConfig (additionalKeys)
import System.Environment
import Data.Semigroup

--------------------------------------------------------------------------------
main :: IO ()
main = getArgs >>= \case
    ["--restart"] -> sendRestart
    _             -> app

app :: IO ()
app = do
  spawn "respawn d12-taffybar"
  launch $
    -- -- docks allows xmonad to handle taffybar
    -- docks $
    -- ewmh allows taffybar access to the state of xmonad/x11
    ewmh $
    -- pagerHints supplies additional state that is not supplied by ewmh
    pagerHints $
    ewmh $
    def
    { -- Use Super instead of Alt
      modMask = mod4Mask

    -- Hooks
    , manageHook      = manageDocks <+> manageHook def
    , layoutHook      = avoidStruts $ layoutHook def
    , handleEventHook = handleEvent <+> handleEventHook def <+> docksEventHook

    -- Java swing applications and xmonad are not friends, so we need to pretend
    -- a little bit
    , startupHook = setWMName "LG3D"

    -- Borders
    , normalBorderColor = "black"
    , focusedBorderColor = "orange"

    -- Workspaces
    , workspaces = [ "1:emacs"
                   , "2:term"
                   , "3:web"
                   , "4:chat"
                   , "5:media"
                   , "6:other"
                   ]

    -- Unfortunately, use urxvt
    , terminal = "urxvt"
    } `additionalKeys` extraKeys

--------------------------------------------------------------------------------
extraKeys :: [((KeyMask, KeySym), X ())]
extraKeys =
  [ ((mod4Mask, xK_q), rebuild)
  , ((0, xF86XK_AudioRaiseVolume), vlmInc)
  , ((0, xF86XK_AudioLowerVolume), vlmDec)
  , ((0, xF86XK_AudioMute), vlmMute)
  , ((0, xF86XK_AudioMicMute), micMute)
  , ((mod4Mask .|. shiftMask, xK_z), spawn "xlocker")
  , ((mod4Mask, xK_Escape), spawn "switch_kbd_layout")
  , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s") -- one window
  , ((0, xK_Print), spawn "scrot")
  ]

--------------------------------------------------------------------------------
sendRestart :: IO ()
sendRestart = do
  dpy <- openDisplay ""
  rw <- rootWindow dpy $ defaultScreen dpy
  xmonad_restart <- internAtom dpy "D12_XMONAD_RESTART" False
  allocaXEvent $ \e -> do
      setEventType e clientMessage
      setClientMessageEvent e rw xmonad_restart 32 0 currentTime
      sendEvent dpy rw False structureNotifyMask e
  sync dpy False

--------------------------------------------------------------------------------
rebuild :: X ()
rebuild = spawn "eru xmonad"

--------------------------------------------------------------------------------
handleEvent :: Event -> X All
handleEvent e@ClientMessageEvent { ev_message_type = mt } = do
  a <- getAtom "D12_XMONAD_RESTART"
  if (mt == a)
    then restart "d12-xmonad" True >> pure (All True)
    else broadcastMessage e >> pure (All True)
handleEvent e = broadcastMessage e >> pure (All True)

--------------------------------------------------------------------------------
vlmInc :: MonadIO m => m ()
vlmInc = spawn "pactl set-sink-mute 0 false ; pactl set-sink-volume 0 +5%"

vlmDec :: MonadIO m => m ()
vlmDec = spawn "pactl set-sink-mute 0 false ; pactl set-sink-volume 0 -5%"

vlmMute :: MonadIO m => m ()
vlmMute = spawn "pactl set-sink-mute 0 toggle"

micMute :: MonadIO m => m ()
micMute = spawn "pactl set-source-mute 1 toggle"
