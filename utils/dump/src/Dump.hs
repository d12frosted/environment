{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Dump
  (run
  ,Action(..)
  ,ActionType(..)) where

import BasicPrelude hiding (FilePath, empty)
import Control.Monad.Except
import Data.Text as Text (replace, dropWhileEnd)
import Data.Time
import Turtle

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Action = Dump ActionType | Restore
data ActionType = AMove | ACopy
type AppM = ExceptT Text IO

--------------------------------------------------------------------------------
-- Runner
--------------------------------------------------------------------------------

run :: Action -> FilePath -> IO ()
run act path' =
  do app <-
       runExceptT $
       case act of
         Restore -> restore path
         Dump act' -> dump act' path
     case app of
       Left errMsg -> error . textToString $ errMsg
       Right () -> return ()
  where path = dropTrailingSlash path'

--------------------------------------------------------------------------------
-- Dump action
--------------------------------------------------------------------------------

dump :: ActionType -> FilePath -> AppM ()
dump act path =
  do targetExists path
     liftIO $ getFormattedTime >>=
       fromAction act path . appendBeforeExtension path

--------------------------------------------------------------------------------
-- Restore
--------------------------------------------------------------------------------

restore :: FilePath -> AppM ()
restore path =
  do targetExists path
     if length matched == 1
        then liftIO $ fromAction AMove path newPath
        else throwError errMsg
  where file = toText' $ filename path
        dir = directory path
        matched = match (has timePattern) file
        errMsg = "Target doesn't have time in it's name or it's in a wrong format!"
        newPath = dir <> (fromText . replace (head matched) "" $ file)

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

timeFormatter :: IsString a => a
timeFormatter = "_%Y_%m_%d_%H_%M_%S"

timePattern :: Pattern Text
timePattern = datePart 4 <> datePart 2 <> datePart 2 <> datePart 2 <> datePart 2 <> datePart 2
  where datePart n = once (char '_') <> fixed n (star digit)

getFormattedTime :: IsString a => IO a
getFormattedTime = liftM formatTime' getTime
  where formatTime' = fromString . formatTime defaultTimeLocale timeFormatter

getTime :: IO LocalTime
getTime =
  do t <- getCurrentTime
     z <- getCurrentTimeZone
     return $ utcToLocalTime z t

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

targetExists :: FilePath -> AppM ()
targetExists path =
  do fileExists <- testfile path
     dirExists <- testdir path
     unless (fileExists || dirExists)
            (throwError "Target doesn't exist!")

fromAction :: MonadIO m => ActionType -> FilePath -> FilePath -> m ()
fromAction ACopy = \p1 p2 -> void . proc "cp" ["-r", toText' p1, toText' p2] $ empty
fromAction AMove = mv

dropTrailingSlash :: FilePath -> FilePath
dropTrailingSlash = fromText . Text.dropWhileEnd (== '/') . toText'

appendBeforeExtension :: FilePath -> Text -> FilePath
appendBeforeExtension path t = fromText $ name <> t <> ext
  where name = toText' $ dropExtension path
        ext = maybe "" ("." <>) . extension $ path

toText' :: FilePath -> Text
toText' = either (error . textToString) id . toText
