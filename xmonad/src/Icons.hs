-- | Utility functions for working with icons (Unicode or Font Awesome
-- characters).

--------------------------------------------------------------------------------
module Icons ( static
             , inactive
             , inactiveThin
             , active
             ) where

--------------------------------------------------------------------------------
import Colors

--------------------------------------------------------------------------------
import XMonad.Hooks.DynamicLog (xmobarColor, wrap)

--------------------------------------------------------------------------------
static :: String -> String
static = xmobarColor iconStaticFg iconStaticBg . thin

inactiveThin :: String -> String
inactiveThin = xmobarColor iconInactiveFg iconInactiveBg . thin

inactive :: String -> String
inactive = xmobarColor iconInactiveFg iconInactiveBg . wide

active :: String -> String
active = xmobarColor iconActiveFg iconActiveBg . wide

--------------------------------------------------------------------------------
wide :: String -> String
wide = wrap " " " "

thin :: String -> String
thin = wrap "\x2008" "\x2008"
