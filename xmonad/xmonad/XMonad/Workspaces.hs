-- | Configurations for workspaces.

--------------------------------------------------------------------------------
module XMonad.Workspaces where

--------------------------------------------------------------------------------
import Data.Semigroup
import XMonad

--------------------------------------------------------------------------------
wsCode1 :: WorkspaceId
wsCode1 = "\xf121"

wsCode2 :: WorkspaceId
wsCode2 = "\xf120"

wsWeb :: WorkspaceId
wsWeb = "\xf269"

wsChat :: WorkspaceId
wsChat = "\xf086"

wsMail :: WorkspaceId
wsMail = "\xf0e0"

wsMedia :: WorkspaceId
wsMedia = "\xf001"

wsOther :: WorkspaceId
wsOther = "\xf18c"

--------------------------------------------------------------------------------
manageAppsWorkspace :: Query (Endo WindowSet)
manageAppsWorkspace
  = composeAll . concat $
    [ [ className =? "Firefox" --> doShift wsWeb ]
    , [ className =? "Nightly" --> doShift wsWeb ]
    , [ className =? "jetbrains-idea" --> doShift wsCode2 ]
    , [ className =? "Spotify" --> doShift wsMedia ]
    , [ className =? "TelegramDesktop" --> doShift wsChat ]
    , [ className =? "Slack" --> doShift wsChat ]
    , [ className =? "Thunderbird" --> doShift wsMail ]
    ]
