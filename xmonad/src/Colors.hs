-- | Defines colours used in the XMonad and XMobar configurations.

--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

--------------------------------------------------------------------------------
module Colors where

--------------------------------------------------------------------------------
import Data.String

--------------------------------------------------------------------------------
type Color = forall a. IsString a => a

--------------------------------------------------------------------------------
background :: Color
background = "#fefefe"

backgroundInactive :: Color
backgroundInactive = background

backgroundActive :: Color
backgroundActive = "#ca3435"

--------------------------------------------------------------------------------
textRegular :: Color
textRegular = "#050505"

textWarning :: Color
textWarning = "#e77200"

textAlert :: Color
textAlert = "#ca3435"

textFocusedBg :: Color
textFocusedBg = textRegular

textFocusedFg :: Color
textFocusedFg = background

textTitleBg :: Color
textTitleBg = ""

textTitleFg :: Color
textTitleFg = "#4848ff"

--------------------------------------------------------------------------------
iconBg :: Color
iconBg = "#f4f4f4"

iconFg :: Color
iconFg = textRegular

iconFocusedBg :: Color
iconFocusedBg = iconFg

iconFocusedFg :: Color
iconFocusedFg = iconBg
