{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           BasicPrelude hiding (FilePath, empty)
import           Data.Time
import           Data.Version (showVersion)
import           Options.Applicative.Simple as OptApp
import qualified Paths_dump as Meta
import           Turtle (testfile, testdir, mv, proc, toText, fromText, FilePath, dropExtension, extension, empty)

data Action = AMove | ACopy deriving Show

--------------------------------------------------------------------------------
-- Chain class
--------------------------------------------------------------------------------

class Chain a where
  chain :: a -> a -> a

instance Chain (Maybe a) where
  Nothing `chain` _ = Nothing
  _       `chain` a = a

instance Chain (Either a b) where
  Left a `chain` _  = Left a
  _      `chain` a  = a

instance Chain a => Chain (IO a) where
  a `chain` b = do a' <- a
                   b' <- b
                   return $ a' `chain` b'

infixl 1 *>>
(*>>) :: Chain a => a -> a -> a
(*>>) = chain

--------------------------------------------------------------------------------
-- Main program
--------------------------------------------------------------------------------

main :: IO ()
main =
  do ((act,path),()) <-
       simpleOptions
         (showVersion Meta.version)
         "dump"
         "Utility for dumping files."
         ((,) <$>
          flag AMove
               ACopy
               (long "copy" <> short 'c' <>
                help "Copy target instead of moving") <*>
          strArgument (metavar "FILE_OR_DIR"))
        OptApp.empty
     liftIO (dump act (fromString path)) >>=
      \case Right () -> return ()
            Left msg -> error . textToString $ msg

dump :: Action -> FilePath -> IO (Either Text ())
dump act path =
  validateTarget path *>>
  (getFormattedTime >>=
   liftM Right . fromAction act path . appendBeforeExtension path)

validateTarget :: FilePath -> IO (Either Text ())
validateTarget path =
  do fileExists <- testfile path
     dirExists <- testdir path
     return $ fromBool (fileExists || dirExists) () "FILE_OR_DIR doesn't exist!"

fromAction :: MonadIO m => Action -> FilePath -> FilePath -> m ()
fromAction ACopy = \p1 p2 -> void . proc "cp" ["-r", toText' p1, toText' p2] $ Turtle.empty
fromAction AMove = mv

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

timeFormatter :: IsString a => a
timeFormatter = "%Y_%m_%d_%H_%M_%S"

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

fromBool :: Bool -> a -> b -> Either b a
fromBool p a b = if p then Right a else Left b

appendBeforeExtension :: FilePath -> Text -> FilePath
appendBeforeExtension path t = fromText $ name <> "_" <> t <> ext
  where name = toText' $ dropExtension path
        ext = maybe "" ("." <>) . extension $ path

toText' :: FilePath -> Text
toText' = either (error . textToString) id . toText
