-- | Custom interactive commands that usually run by pressing some magic key
-- bindings.

--------------------------------------------------------------------------------
module XMonad.Commands where

--------------------------------------------------------------------------------
import Control.Monad.IO.Class (MonadIO)
import XMonad

--------------------------------------------------------------------------------
vlmInc :: MonadIO m => m ()
vlmInc = spawn "pactl set-sink-mute 0 false ; pactl set-sink-volume 0 +5%"

vlmDec :: MonadIO m => m ()
vlmDec = spawn "pactl set-sink-mute 0 false ; pactl set-sink-volume 0 -5%"

vlmMute :: MonadIO m => m ()
vlmMute = spawn "pactl set-sink-mute 0 toggle"

micMute :: MonadIO m => m ()
micMute = spawn "pactl set-source-mute 1 toggle"
