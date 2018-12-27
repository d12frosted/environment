import Data.Default (def)
import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ def
    { -- Use Super instead of Alt
      modMask = mod4Mask

    -- Hooks
    , manageHook      = manageDocks <+> manageHook def
    , layoutHook      = avoidStruts  $  layoutHook def
    , handleEventHook = handleEventHook def <+> docksEventHook

    -- Java swing applications and xmonad are not friends, so we need to pretend a
    -- little bit
    , startupHook = setWMName "LG3D"

    , logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "black" "" . shorten 80
                , ppCurrent = xmobarColor "#006000" ""
                }

    -- Borders
    , normalBorderColor = "black"
    , focusedBorderColor = "orange"

    -- Workspaces
    , workspaces = [ "1:emacs"
                   , "2:term"
                   , "3:web"
                   , "4:media"
                   , "5:other"
                   ]
    } `additionalKeys` [ ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-mute 0 false ; pactl set-sink-volume 0 +5%")
                       , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-mute 0 false ; pactl set-sink-volume 0 -5%")
                       , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")
                       , ((0, xF86XK_AudioMicMute), spawn "pactl set-source-mute 1 toggle")
                       ]
