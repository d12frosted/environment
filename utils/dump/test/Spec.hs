{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- module Main where

import BasicPrelude
import Turtle
import Dump

main :: IO ()
main =
  do generateTestData
     cleanup

workingDir :: IsString a => a
workingDir = "test_dir"

generateTestData :: IO ()
generateTestData =
  do mkdir workingDir
     cd workingDir
     mkdir "dir1"
     mkdir "dir2"
     touch "file1"
     touch "dir2/file2"
     touch "file3.txt"
     cd ".."

cleanup :: IO ()
cleanup = rmtree workingDir

--------------------------------------------------------------------------------
-- Test dirs
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Test files
--------------------------------------------------------------------------------
