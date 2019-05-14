-- | Utility functions for working with icons (Unicode or Font Awesome
-- characters).

--------------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

--------------------------------------------------------------------------------
module Icons ( icon
             , iconWide
             , iconWideFocused
             ) where

--------------------------------------------------------------------------------
import Colors

--------------------------------------------------------------------------------
import XMonad.Hooks.DynamicLog (xmobarColor, wrap)

--------------------------------------------------------------------------------
icon :: String -> String
icon = xmobarColor iconFg iconBg . thin

iconWide :: String -> String
iconWide = xmobarColor iconFg iconBg . wide

iconWideFocused :: String -> String
iconWideFocused = xmobarColor iconFocusedFg iconFocusedBg . wide

--------------------------------------------------------------------------------
wide :: String -> String
wide = wrap " " " "

thin :: String -> String
thin = wrap "\x2008" "\x2008"
