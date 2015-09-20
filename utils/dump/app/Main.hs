{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           BasicPrelude hiding (empty)
import           Data.Version (showVersion)
import           Options.Applicative.Simple
import qualified Paths_dump as Meta

import Dump

main :: IO ()
main =
  do ((act,rest,path),()) <-
       simpleOptions
         (showVersion Meta.version)
         "dump"
         "Utility for dumping files."
         ((,,) <$>
          flag AMove
               ACopy
               (long "copy" <> short 'c' <>
                help "Copy target instead of moving") <*>
          flag False
               True
               (long "restore" <> short 'r' <>
                help "Restore target instead of dumping") <*>
          strArgument (metavar "FILE_OR_DIR"))
        empty
     if rest
         then run Restore $ fromString path
         else run (Dump act) $ fromString path
