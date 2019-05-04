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
    font = "xft:Source Code Pro:size=14,Symbola:size=16,FontAwesome:size=14"
  , borderColor = "#fefefe"
  , border = NoBorder
  , bgColor = "#fefefe"
  , fgColor = "#050505"
  , alpha = 255
  , position = TopSize C 100 32

  -- layout
  , sepChar =  "%"   -- delineator between plugin names and straight text
  , alignSep = "}{"  -- separator between left-right alignment
  , template = " %StdinReader%}{\x2328 %kbd% \xf1eb %wlp3s0wi% %default:Master% %battery% %date% "

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
                    , "--low"      , "#ca3435"
                    , "--normal"   , "#e77200"
                    , "--high"     , "#050505"
                    , "--" -- battery specific options
                    -- discharging status
                    , "-o", "\x1F50B\x2620 <left>% (<timeleft>)"
                    -- AC "on" status
                    , "-O", "\x1F50B\x1F5F2 <left>% (<timeleft>)"
                    -- charged status
                    , "-i", "\x1F50B"
                    ] 50

    , Run $ Date "\xf073 %F (%a) \x23F2 %T" "date" 10

    , Run $ Volume "default" "Master" [ "--template", "<status> <volume>%"
                                      ,  "--"
                                      , "-o", "\x1F507"
                                      , "-O", "\x1F50A"
                                      , "-c", "#050505"
                                      , "-C", "#050505"
                                      ] 10

    , Run $ Kbd []

    , Run $ Wireless "wlp3s0" [ "--template", "<essid>"
                              ] 100

    , Run StdinReader
    ]
  }
