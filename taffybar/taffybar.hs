{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Data.Text as T
import           GI.Gtk ( Widget
                        , eventBoxNew
                        , containerAdd
                        , eventBoxSetVisibleWindow
                        , widgetShowAll
                        , toWidget
                        )
import           System.Taffybar
import           System.Taffybar.Hooks
import           System.Taffybar.SimpleConfig
import           System.Taffybar.Util
import           System.Taffybar.Widget
import           System.Taffybar.Widget.Generic.PollingLabel

--------------------------------------------------------------------------------
main :: IO ()
main = startTaffybar $
  withBatteryRefresh $
  withLogServer $
  withToggleServer $
  toTaffyConfig config

--------------------------------------------------------------------------------
config :: SimpleTaffyConfig
config =
  defaultSimpleTaffyConfig
  { startWidgets = workspacesNew workspacesConfig' : map (>>= buildContentsBox)
    [ layoutNew defaultLayoutConfig
    , windowsNew defaultWindowsConfig ]
  , endWidgets = map (>>= buildContentsBox)
    [ textClockNew Nothing "%F (%a) %T" 0.5
    , textBatteryNew "$percentage$% $time$ | "
    , batteryIconNew
    , sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt
    , kbdNew 0.5
    ]
  , barPosition = Top
  , barPadding = 0
  , barHeight = 32
  , widgetSpacing = 0
  , cssPath = Just "/home/borysb/.config/taffybar/taffybar.css"
  }

--------------------------------------------------------------------------------
workspacesConfig' :: WorkspacesConfig
workspacesConfig' =
  defaultWorkspacesConfig
  { minIcons = 1
  , widgetGap = 0
  , showWorkspaceFn = hideEmpty
  }

--------------------------------------------------------------------------------
kbdNew :: MonadIO m => Double -> m Widget
kbdNew updateSeconds = liftIO $ do
  l <- pollingLabelNew updateSeconds getLayoutName
  ebox <- eventBoxNew
  containerAdd ebox l
  eventBoxSetVisibleWindow ebox False
  widgetShowAll ebox
  toWidget ebox
  where
    getLayoutName :: IO T.Text
    getLayoutName = liftA handle $ runCommand "xkblayout-state" ["print", "%s"]
    handle :: Either String String -> T.Text
    handle (Left _) = "[??]"
    handle (Right r) = mconcat ["[", T.pack r, "]"]
