{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
import Control.Monad.IO.Class (MonadIO)
import Data.Default (def)
import Data.Semigroup
import Graphics.X11.ExtraTypes.XF86
import System.Environment
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.LayoutModifier
import XMonad.Util.EZConfig (additionalKeys)

--------------------------------------------------------------------------------
main :: IO ()
main = getArgs >>= \case
    ["--restart"] -> sendRestart
    _             -> app

app :: IO ()
app = launch =<< statusBar "d12-xmobar" statusBarPP toggleStrutsKey xmonadConfig

--------------------------------------------------------------------------------
xmonadConfig :: XConfig (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)))
xmonadConfig
  = def
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

extraKeys :: [((KeyMask, KeySym), X ())]
extraKeys =
  [ ((mod4Mask, xK_q), rebuild)
  , ((mod4Mask, xK_p), spawn "dmenu_run -h 32 -fn 'Source Code Pro-14'")
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
statusBarPP :: PP
statusBarPP
  = def
  { ppCurrent = xmobarColor "#01786f" "" . ("\x23A8 " <>) . (<> " \x23AC")
  , ppTitle = xmobarColor "#4848ff" "" . shorten 120 . ("\x1F5B3 " <>)
  }

--------------------------------------------------------------------------------
toggleStrutsKey :: XConfig l -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = m} = (m, xK_b)

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
    then restart "d12-xmonad" True >> pure (All False)
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
