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
    , layoutHook      = avoidStruts $ layoutHook def
    , handleEventHook = handleEventHook def <+> docksEventHook

    -- Java swing applications and xmonad are not friends, so we need to pretend
    -- a little bit
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
                   , "4:chat"
                   , "5:media"
                   , "6:other"
                   ]
    } `additionalKeys` extraKeys


--------------------------------------------------------------------------------
extraKeys :: MonadIO m => [((KeyMask, KeySym), m ())]
extraKeys =
  [ ((0, xF86XK_AudioRaiseVolume), vlmInc)
  , ((0, xF86XK_AudioLowerVolume), vlmDec)
  , ((0, xF86XK_AudioMute), vlmMute)
  , ((0, xF86XK_AudioMicMute), micMute)
  , ((mod4Mask .|. shiftMask, xK_z), spawn "xlocker")
  , ((mod4Mask, xK_Escape), spawn "switch_kbd_layout")
  , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s") -- one window
  , ((0, xK_Print), spawn "scrot")
  ]

--------------------------------------------------------------------------------
vlmInc :: MonadIO m => m ()
vlmInc = spawn "pactl set-sink-mute 0 false ; pactl set-sink-volume 0 +5%"

vlmDec :: MonadIO m => m ()
vlmDec = spawn "pactl set-sink-mute 0 false ; pactl set-sink-volume 0 -5%"

vlmMute :: MonadIO m => m ()
vlmMute = spawn "pactl set-sink-mute 0 toggle"

micMute :: MonadIO m => m ()
micMute = spawn "pactl set-source-mute 1 toggle"
