{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

--------------------------------------------------------------------------------

import Control.Exception (SomeException (..), handle)
import Data.List (isPrefixOf)
import Path.Parse
import System.Exit
import System.IO
import System.Process (runInteractiveProcess, waitForProcess)
import qualified Utils.Color as Color
import qualified Utils.Icon as Icon
import Xmobar

--------------------------------------------------------------------------------

newtype Env = Env
  { envConfigHome :: Path Abs Dir
  }

--------------------------------------------------------------------------------

main :: IO ()
main = do
  env <- Env <$> parseDirPath "$XDG_CONFIG_HOME"
  xmobar $ config env

--------------------------------------------------------------------------------

config :: Env -> Config
config env =
  defaultConfig
    { -- appearance
      font = "xft:Source Code Pro:size=10,Symbola:size=10,FontAwesome:size=10",
      border = NoBorder,
      borderColor = Color.background,
      bgColor = Color.background,
      fgColor = Color.textRegular,
      alpha = 255,
      position = TopSize C 100 32,
      -- layout
      sepChar = "%", -- delineator between plugin names and straight text
      alignSep = "}{", -- separator between left-right alignment
      template =
        concat
          [ "%StdinReader%",
            "}{",
            "%dropbox-status%",
            " ",
            "%default:Master%",
            " ",
            "%default:Capture%",
            " ",
            Icon.static "\x2328" <> " %kbd%",
            " ",
            Icon.static "\xf1eb" <> " %wlp3s0wi%",
            " ",
            "%battery%",
            " ",
            "%date%",
            " ",
            "%notification-status%",
            " "
          ],
      -- general behavior
      lowerOnStart = True, -- send to bottom of window stack on start
      hideOnStart = False, -- start with window unmapped (hidden)
      allDesktops = True, -- show on all desktops
      overrideRedirect = True, -- set the Override Redirect flag (Xlib)
      pickBroadest = False, -- choose widest display (multi-monitor)
      persistent = True, -- enable/disable hiding (True = disabled)

      -- icons
      iconRoot = toFilePath $ envConfigHome env </> [reldir|xmonad/icons|],
      -- plugins
      commands =
        [ Run $
            Battery
              [ "--template",
                "<acstatus>",
                "--Low",
                "20", -- units: %
                "--High",
                "80", -- units: %
                "--low",
                Color.textAlert,
                "--normal",
                Color.textWarning,
                "--high",
                Color.textRegular,
                "--", -- battery specific options
                -- discharging status
                "-o",
                Icon.static "\xf242" <> " <left>% (<timeleft>)",
                -- AC "on" status
                "-O",
                Icon.static "\xf0e7" <> " <left>% (<timeleft>)",
                -- charged status
                "-i",
                Icon.static "\xf240"
              ]
              50,
          Run $ Date dateTemplate "date" 10,
          Run $
            Volume
              "default"
              "Master"
              [ "--template",
                "<status> <volume>%",
                "--",
                "-o",
                Icon.static "\x1F507",
                "-O",
                Icon.static "\x1F50A",
                "-c",
                Color.textRegular,
                "-C",
                Color.textRegular
              ]
              10,
          Run $
            Volume
              "default"
              "Capture"
              [ "--template",
                "<status> <volume>%",
                "--",
                "-o",
                Icon.static "\xf131",
                "-O",
                Icon.static "\xf130",
                "-c",
                Color.textRegular,
                "-C",
                Color.textRegular
              ]
              10,
          Run $ Kbd [],
          Run $
            Wireless
              "wlp3s0"
              [ "--template",
                "<essid>"
              ]
              100,
          Run $ NotificationStatus 10,
          Run $ DropboxStatus 100,
          Run StdinReader
        ]
    }

dateTemplate :: String
dateTemplate =
  concat
    [ Icon.static "\xf073",
      " %F (%a) ",
      Icon.static "\x23F2",
      " %T"
    ]

--------------------------------------------------------------------------------

newtype NotificationStatus = NotificationStatus Int deriving (Show, Read)

instance Exec NotificationStatus where
  alias _ = "notification-status"
  start (NotificationStatus r) callback = pollProg r "notify" ["status"] cb
    where
      cb (Just "enabled") = callback $ Icon.static "\xf0f3"
      cb (Just "disabled") = callback $ Icon.alert "\xf1f6"
      cb _ = callback "?"

--------------------------------------------------------------------------------

newtype DropboxStatus = DropboxStatus Int deriving (Show, Read)

instance Exec DropboxStatus where
  alias _ = "dropbox-status"
  start (DropboxStatus r) callback = pollProg r "dropbox" ["status"] cb
    where
      cb Nothing = callback "?"
      cb (Just res)
        | res == "Dropbox isn't running!" =
          callback $ Icon.alert "\xf16b Not running"
        | res == "Up to date" =
          callback $ Icon.static "\xf16b"
        | "Syncing" `isPrefixOf` res =
          callback $ Icon.static "\xf16b \xf021"
        | "Connecting" `isPrefixOf` res =
          callback $ Icon.alert "\xf16b \xf071 No connection"
        | otherwise =
          callback $ Icon.alert "\xf16b \xf071 Unknown"

--------------------------------------------------------------------------------

pollProg :: Int -> FilePath -> [String] -> (Maybe String -> IO ()) -> IO ()
pollProg interval prog args cb = if interval > 0 then go else exec >>= cb
  where
    go = exec >>= cb >> tenthSeconds interval >> go
    exec = execProg' prog args

execProg' :: FilePath -> [String] -> IO (Maybe String)
execProg' prog args = do
  (i, o, e, p) <- runInteractiveProcess prog args Nothing Nothing
  exit <- waitForProcess p
  let closeHandles = hClose o >> hClose i >> hClose e
      getL =
        handle
          (\(SomeException _) -> return "")
          (hGetLine o)
  case exit of
    ExitSuccess -> do
      str <- getL
      closeHandles
      pure (Just str)
    _ -> closeHandles >> pure Nothing

--------------------------------------------------------------------------------
