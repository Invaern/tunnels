module Ui.Model where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Data.Word (Word16)
import Brick.Widgets.List (list, List)
import qualified Data.Vector as V
import qualified Config as CFG
import Control.Exception (Exception)

data Tunnel = Tunnel {
    name :: String
  , tunnelThrough :: String
  , remoteHost :: String
  , localPort :: Word16
  , remotePort :: Word16
  , status ::  TVar TunnelStatus
  , uStatus :: TunnelStatus
}

data TunnelStatus = Active ThreadId | Inactive | Killed | Starting | Restarting deriving (Eq)

data AppEvent = LogMsg Msg | Refresh

data AppResource = Tunnels | Logs deriving (Eq, Ord, Show)

data Msg = Msg {
    mName :: String
  , mTime :: String
  , mLine :: String
}

data AppState = AppState {
    tunnels :: List AppResource Tunnel
  , logs :: List AppResource Msg
  , active :: Bool
}

data AppException = RestartRequested deriving (Show)
instance Exception AppException

toAppState :: CFG.Config -> IO AppState
toAppState cfg = do
    tunnels <- mapM toTunnel (CFG.tunnels cfg)
    let tunnelsList = list Tunnels (V.fromList tunnels) 1
        logsList = list Logs V.empty 1
    pure $ AppState tunnelsList logsList True

toTunnel :: CFG.TunnelConf -> IO Tunnel
toTunnel tunnelConf = do
    status <- newTVarIO Inactive
    pure Tunnel {
        name = CFG.name tunnelConf
      , tunnelThrough = CFG.tunnelThrough tunnelConf
      , remoteHost = CFG.remoteHost tunnelConf
      , localPort = CFG.localPort tunnelConf
      , remotePort = CFG.remotePort tunnelConf
      , status = status
      , uStatus = Inactive
    }