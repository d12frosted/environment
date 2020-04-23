--------------------------------------------------------------------------------

-- |
-- Module      : Melkor
-- Description : Entry point for Melkor
-- Copyright   : (c) Boris Buliga, 2020
-- License     : MIT
-- Maintainer  : boris@d12frosted.io
-- Stability   : experimental
-- Portability : POSIX
module Melkor
  (
  )
where

--------------------------------------------------------------------------------

import Melkor.BuildPlan
import Melkor.Types.Provider (Provider)
import qualified Melkor.Types.Provider as Provider
import Melkor.Types.Resource (Resource)
import qualified Melkor.Types.Resource as Resource
import RIO

--------------------------------------------------------------------------------
