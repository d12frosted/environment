--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

--------------------------------------------------------------------------------

-- | Defines colours used in the XMonad and XMobar configurations.
module Utils.Color where

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
textAlert = "#962727"

textFocusedBg :: Color
textFocusedBg = textRegular

textFocusedFg :: Color
textFocusedFg = background

textTitleBg :: Color
textTitleBg = ""

textTitleFg :: Color
textTitleFg = "#4848ff"

--------------------------------------------------------------------------------

iconStaticBg :: Color
iconStaticBg = "#f3fcf3"

iconStaticFg :: Color
iconStaticFg = textRegular

iconInactiveBg :: Color
iconInactiveBg = background

iconInactiveFg :: Color
iconInactiveFg = textRegular

iconActiveBg :: Color
iconActiveBg = "#bfdec8"

iconActiveFg :: Color
iconActiveFg = textRegular

iconAlertBg :: Color
iconAlertBg = "#fef0f0"

iconAlertFg :: Color
iconAlertFg = textRegular

--------------------------------------------------------------------------------

popupBg :: Color
popupBg = "#f3fcf3"

popupFg :: Color
popupFg = "#050505"

--------------------------------------------------------------------------------
