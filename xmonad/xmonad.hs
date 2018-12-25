import Data.Default (def)
import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.Run (spawnPipe)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad def
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
    }
