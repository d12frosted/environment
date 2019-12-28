-- | Impatient with the emptiness of the Great Void outside the Timeless Halls,
-- and desiring to create things of his own, Melkor often went forth into the
-- Void in search of the Flame Imperishable. But the Flame was of Iluvatar and
-- resided with him...

--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import           Melkor.Core

--------------------------------------------------------------------------------

import           RIO

--------------------------------------------------------------------------------

main :: IO ()
main = runMelkor melkor

melkor :: RIO Melkor ()
melkor = do
  logInfo "Melkor is here!"

  -- dependencies
  ghRepo "d12frosted" "hledger-imp" "$DEVELOPER/hledger-imp"

  -- providers
  logDebug "no providers yet"
  addProvider repoProvider

--------------------------------------------------------------------------------
