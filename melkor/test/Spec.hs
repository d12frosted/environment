--------------------------------------------------------------------------------

-- |
-- Module      : Spec
-- Description : Melkor test spec
-- Copyright   : (c) Boris Buliga, 2020
-- License     : MIT
-- Maintainer  : boris@d12frosted.io
-- Stability   : experimental
-- Portability : POSIX
module Main where

--------------------------------------------------------------------------------

import Melkor.BuildMapSpec as BuildMap
import Melkor.BuildPlanSpec as BuildPlan
import Melkor.Types.Internal.SingleRelMapSpec as SingleRelMap
--------------------------------------------------------------------------------

import RIO
import Test.Tasty

--------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ SingleRelMap.tests,
        BuildMap.tests
        -- , BuildPlan.tests
      ]

--------------------------------------------------------------------------------
