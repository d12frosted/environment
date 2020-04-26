--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Melkor
import Melkor.Types.Provider
import Melkor.Types.Resource
import RIO
import qualified RIO.HashSet as HS

--------------------------------------------------------------------------------

main :: IO ()
main = runMelkor melkor

melkor :: RIO Melkor ()
melkor = do
  depend $ gitHubRepo "d12frosted" "hledger-imp"
  depend $ gitHubRepo "d12frosted" "environment"
  using dummyProvider
  using smartProvider

--------------------------------------------------------------------------------

dummyProvider :: Provider
dummyProvider =
  mkProvider
    "GitHub"
    HS.empty
    (const (pure True))
    (const (pure Missing))
    (const (pure ()))
    (const (pure ()))

smartProvider :: Provider
smartProvider =
  mkProvider
    "GitLab"
    HS.empty
    (const (pure True))
    (const (pure Missing))
    (const (pure ()))
    (const (pure ()))
