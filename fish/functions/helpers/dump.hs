{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import BasicPrelude
import Data.Text as Text (last, null, init)
import Data.Time
import Filesystem.Path
import Filesystem.Path.CurrentOS
import System.Directory (renameDirectory, renameFile, doesFileExist, doesDirectoryExist)
import System.Locale

main :: IO ()
main =
  do args <- getArgs
     case args of
       [] -> error "Expected file or directory, but got nothing."
       (target:_) ->
         do time <- getTime
            let time' = fromString $ formatTime defaultTimeLocale "%Y_%m_%d_%H:%M:%S" time
                name = rename (validate target) time'
              in renameFileOrDirectory target name

validate :: Text -> Text
validate target =
  case Text.null target of
    True -> error "Target is empty."
    False -> case Text.last target of
               '/' -> Text.init target
               _ -> target

rename :: Text -> Text -> Text
rename target time = name ++ "_" ++ time ++ ext
  where target' = fromText target
        name = toText' $ dropExtensions target'
        ext = case extensions target' of
                [] -> ""
                xs -> "." ++ intercalate "." xs
        toText' = either error' id . toText
        error' = error . textToString

getTime :: IO LocalTime
getTime =
  do t <- getCurrentTime
     z <- getCurrentTimeZone
     return $ utcToLocalTime z t

renameFileOrDirectory :: Text -> Text -> IO ()
renameFileOrDirectory oldName newName =
  let oldName' = textToString oldName
      newName' = textToString newName
    in do isFile <- doesFileExist oldName'
          case isFile of
            True -> renameFile oldName' newName'
            False ->
              do isDir <- doesDirectoryExist oldName'
                 case isDir of
                   True -> renameDirectory oldName' newName'
                   False -> error $ "File or directory doesn't exist: " ++ oldName'
