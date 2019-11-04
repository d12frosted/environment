-- | Utility functions for working with icons (Unicode or Font Awesome
-- characters).

--------------------------------------------------------------------------------

module Utils.Icon
  ( static
  , inactive
  , inactiveThin
  , active
  , alert
  ) where

--------------------------------------------------------------------------------

import           Utils.Color

--------------------------------------------------------------------------------

import           XMonad.Hooks.DynamicLog (wrap, xmobarColor)

--------------------------------------------------------------------------------

static :: String -> String
static = xmobarColor iconStaticFg iconStaticBg . thin

inactiveThin :: String -> String
inactiveThin = xmobarColor iconInactiveFg iconInactiveBg . thin

inactive :: String -> String
inactive = xmobarColor iconInactiveFg iconInactiveBg . wide

active :: String -> String
active = xmobarColor iconActiveFg iconActiveBg . wide

alert :: String -> String
alert = xmobarColor iconAlertFg iconAlertBg  . thin

--------------------------------------------------------------------------------

wide :: String -> String
wide = wrap " " " "

thin :: String -> String
thin = wrap "\x2008" "\x2008"

--------------------------------------------------------------------------------
