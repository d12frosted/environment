--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import qualified Utils.Color as Color
import qualified Utils.Icon as Icon
import           XMonad.Keybindings
import           XMonad.Workspaces

--------------------------------------------------------------------------------

import           Data.Default (def)
import           Data.Semigroup
import           System.Environment
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.LayoutModifier
import           XMonad.Util.EZConfig (additionalKeysP)

--------------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= \case
  ["--restart"] -> sendRestart
  _             -> launch =<<
    statusBar "d12-xmobar" statusBarPP toggleStrutsKey xmonadConfig

--------------------------------------------------------------------------------

type XMonadConfig
  = XConfig
    ( ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full))
    )

xmonadConfig :: XMonadConfig
xmonadConfig
  = def
  { -- Use Super instead of Alt
    modMask = mod4Mask

  -- Hooks
  , manageHook      = manageDocks <+> manageAppsWorkspace <+> manageHook def
  , layoutHook      = avoidStruts $ layoutHook def
  , handleEventHook = handleEvent <+> handleEventHook def <+> docksEventHook

  -- Java swing applications and xmonad are not friends, so we need to pretend
  -- a little bit
  , startupHook = setWMName "LG3D"

  -- Borders
  , normalBorderColor = Color.backgroundInactive
  , focusedBorderColor = Color.backgroundActive

  -- Workspaces
  , workspaces = [ wsCode1
                 , wsCode2
                 , wsWeb
                 , wsChat
                 , wsMedia
                 , wsOther
                 ]

  , terminal = "alacritty"
  } `additionalKeysP` keybindings

--------------------------------------------------------------------------------

statusBarPP :: PP
statusBarPP
  = def
  { ppCurrent = Icon.active
  , ppHidden = Icon.inactive
  , ppWsSep = ""
  , ppTitle =
      xmobarColor Color.textTitleFg Color.textTitleBg
      . shorten 120
  , ppLayout = \case
      "Tall"        -> Icon.inactiveThin "\x25E7"
      "Mirror Tall" -> Icon.inactiveThin "\x2B12"
      "Full"        -> Icon.inactiveThin "\x23F9"
      l             -> l
  , ppSep = " "
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

handleEvent :: Event -> X All
handleEvent e@ClientMessageEvent { ev_message_type = mt } = do
  a <- getAtom "D12_XMONAD_RESTART"
  if mt == a
    then restart "d12-xmonad" True >> pure (All False)
    else broadcastMessage e >> pure (All True)
handleEvent e = broadcastMessage e >> pure (All True)

--------------------------------------------------------------------------------
