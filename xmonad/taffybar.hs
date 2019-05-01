{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Int (Int64)
import           Data.String
import qualified Data.Text as T
import           GI.Gtk ( Widget
                        , containerAdd
                        , eventBoxNew
                        , eventBoxSetVisibleWindow
                        , labelNew
                        , labelSetMarkup
                        , onWidgetRealize
                        , toWidget
                        , widgetShowAll
                        )
import           Path.Parse
import           System.Taffybar
import           System.Taffybar.Context (TaffyIO)
import           System.Taffybar.Hooks
import           System.Taffybar.Information.Battery
import           System.Taffybar.SimpleConfig
import           System.Taffybar.Util
import           System.Taffybar.Widget
import           System.Taffybar.Widget.Generic.ChannelWidget
import           System.Taffybar.Widget.Generic.PollingLabel
import           Text.Printf

--------------------------------------------------------------------------------
data Env
  = Env
  { envConfigHome :: Path Abs Dir
  }

--------------------------------------------------------------------------------
main :: IO ()
main = do
  env <- Env <$> parseDirPath "$XDG_CONFIG_HOME"
  startTaffybar
    . withBatteryRefresh
    . withLogServer
    . withToggleServer
    . toTaffyConfig
    $ config env

--------------------------------------------------------------------------------
config :: Env -> SimpleTaffyConfig
config env =
  defaultSimpleTaffyConfig
  { startWidgets = workspacesNew workspacesConfig' : map (>>= buildContentsBox)
    [ layoutNew defaultLayoutConfig
    ]
  , centerWidgets = map (>>= buildContentsBox)
    [ windowsNew defaultWindowsConfig
    ]
  , endWidgets = map (>>= buildContentsBox)
    [ textClockNew Nothing "%F (%a) %T" 0.5
    , textBatteryNew' formatBatteryInfo
    , batteryIconNew
    , sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt
    , kbdNew 0.5
    ]
  , barPosition = Top
  , barPadding = 0
  , barHeight = 32
  , widgetSpacing = 0
  , cssPath = Just . toFilePath $
    envConfigHome env </> [relfile|xmonad/taffybar.css|]
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

--------------------------------------------------------------------------------
data BatteryWidgetInfo = BWI
  { seconds :: Maybe Int64
  , percent :: Int
  , state :: BatteryState
  } deriving (Eq, Show)

textBatteryNew' :: (BatteryWidgetInfo -> T.Text) -> TaffyIO Widget
textBatteryNew' format = do
  chan <- getDisplayBatteryChan
  ctx <- ask
  let getLabelText info = format (getBatteryWidgetInfo info)
      getBatteryInfoIO = runReaderT getDisplayBatteryInfo ctx
  liftIO $ do
    label' <- getLabelText <$> getBatteryInfoIO >>= labelNew . Just
    let setMarkup text = postGUIASync $ labelSetMarkup label' text
        updateWidget' = setMarkup . getLabelText
    void $ onWidgetRealize label' $ getLabelText <$> getBatteryInfoIO >>= setMarkup
    toWidget =<< channelWidgetNew label' chan updateWidget'

getBatteryWidgetInfo :: BatteryInfo -> BatteryWidgetInfo
getBatteryWidgetInfo info
  = BWI
  { seconds = battTime
  , percent = battPctNum
  , state = batteryState info
  }
  where
    battPctNum :: Int
    battPctNum = floor (batteryPercentage info)
    battTime :: Maybe Int64
    battTime = case batteryState info of
      BatteryStateCharging -> Just $ batteryTimeToFull info
      BatteryStateDischarging -> Just $ batteryTimeToEmpty info
      _ -> Nothing

formatBatteryInfo :: IsString a => BatteryWidgetInfo -> a
formatBatteryInfo (BWI _ _ BatteryStateFullyCharged) = ""
formatBatteryInfo (BWI Nothing p _) = fromString $ mconcat [show p, "%"]
formatBatteryInfo (BWI s p _) = fromString $ mconcat [show p, "%", "(", formatDuration s, ")"]

formatDuration :: Maybe Int64 -> String
formatDuration Nothing = ""
formatDuration (Just secs) = let minutes = secs `div` 60
                                 hours = minutes `div` 60
                                 minutes' = minutes `mod` 60
                             in fromString $ printf "%02d:%02d" hours minutes'
