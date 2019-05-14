{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
import qualified Colors
import           Icons

--------------------------------------------------------------------------------
import           Xmobar
import           Path.Parse

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
  , border = NoBorder
  , borderColor = Colors.background
  , bgColor = Colors.background
  , fgColor = Colors.textRegular
  , alpha = 255
  , position = TopSize C 100 32

  -- layout
  , sepChar =  "%"   -- delineator between plugin names and straight text
  , alignSep = "}{"  -- separator between left-right alignment
  , template = concat
    [ "%StdinReader%"
    , "}{"
    , Icons.static "\x2328" <> " %kbd%"
    , " "
    , Icons.static "\xf1eb" <> " %wlp3s0wi%"
    , " "
    , "%default:Master%"
    , " "
    , "%battery%"
    , " "
    , "%date%"
    , " "
    ]

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
                    , "--Low"      , "20"        -- units: %
                    , "--High"     , "80"        -- units: %
                    , "--low"      , Colors.textAlert
                    , "--normal"   , Colors.textWarning
                    , "--high"     , Colors.textRegular
                    , "--" -- battery specific options
                    -- discharging status
                    , "-o", Icons.static "\xf242" <> " <left>% (<timeleft>)"
                    -- AC "on" status
                    , "-O", Icons.static "\xf0e7" <> " <left>% (<timeleft>)"
                    -- charged status
                    , "-i", "\xf240"
                    ] 50

    , Run $ Date dateTemplate "date" 10

    , Run $ Volume "default" "Master" [ "--template", "<status> <volume>%"
                                      ,  "--"
                                      , "-o", Icons.static "\x1F507"
                                      , "-O", Icons.static "\x1F50A"
                                      , "-c", Colors.textRegular
                                      , "-C", Colors.textRegular
                                      ] 10

    , Run $ Kbd []

    , Run $ Wireless "wlp3s0" [ "--template", "<essid>"
                              ] 100

    , Run StdinReader
    ]
  }

dateTemplate :: String
dateTemplate
  = concat
  [ Icons.static "\xf073"
  , " %F (%a) "
  , Icons.static "\x23F2"
  , " %T"
  ]
