{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
import Xmobar
import Path.Parse

--------------------------------------------------------------------------------
data Env = Env { envConfigHome :: Path Abs Dir }

--------------------------------------------------------------------------------
main :: IO ()
main = do
  env <- Env <$> parseDirPath "$XDG_CONFIG_HOME"
  xmobar $ config env

--------------------------------------------------------------------------------
config :: Env -> Config
config env = defaultConfig {
  -- appearance
    font = "xft:Source Code Pro:size=14"
  , borderColor = "#fefefe"
  , border = NoBorder
  , bgColor = "#fefefe"
  , fgColor = "#050505"
  , alpha = 255
  , position = Top

  -- layout
  , sepChar =  "%"   -- delineator between plugin names and straight text
  , alignSep = "}{"  -- separator between left-right alignment
  , template = " %StdinReader%}{[%kbd%] %default:Master% %battery% %date% "

  -- general behavior
  , lowerOnStart =     True    -- send to bottom of window stack on start
  , hideOnStart =      False   -- start with window unmapped (hidden)
  , allDesktops =      True    -- show on all desktops
  , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
  , pickBroadest =     False   -- choose widest display (multi-monitor)
  , persistent =       True    -- enable/disable hiding (True = disabled)

  -- icons
  , iconRoot = toFilePath $ envConfigHome env </> [reldir|xmonad/icons|]

  -- plugins
  , commands =
    [ Run $ Battery [ "--template" , "<acstatus>"
                    , "--Low"      , "10"        -- units: %
                    , "--High"     , "80"        -- units: %
                    , "--low"      , "darkred"
                    , "--normal"   , "darkorange"
                    , "--high"     , "#050505"

                    , "--" -- battery specific options
                    -- discharging status
                    , "-o", "<icon=battery-use.xpm/> <left>% (<timeleft>)"
                    -- AC "on" status
                    , "-O", "<icon=battery-charge.xpm/> <left>% (<timeleft>)"
                    -- charged status
                    , "-i", "<icon=battery-charge.xpm/>"
                    ] 50

    , Run $ Date "%F (%a) %T" "date" 10

    , Run $ Volume "default" "Master" [ "--template", "<status><volume>%"

                                      ,  "--"
                                      , "-o", "<icon=volume-mute.xpm/> "
                                      , "-O", "<icon=volume-up.xpm/> "
                                      , "-C", "#050505"
                                      , "-c", "#050505"
                                      ] 10

    , Run $ Kbd []

    , Run StdinReader
    ]
  }
