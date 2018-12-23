import XMonad
-- import XMonad.Hooks.SetWMName

--------------------------------------------------------------------------------
main
  = xmonad defaultConfig
  { modMask = mod4Mask -- Use Super instead of Alt

  -- Java swing applications and xmonad are not friends
  -- , startupHook = setWMName "LG3D"
  }
