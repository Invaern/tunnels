{-# LANGUAGE ScopedTypeVariables #-}
module Tunnel where

import Ui.Model
import Control.Concurrent (ThreadId)
import GHC.Conc (atomically, readTVar, writeTVar, forkIO)
import Control.Monad (unless, forever, when)
import qualified System.Process as PS
import Brick.BChan (BChan, writeBChan)
import Data.Time.LocalTime
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Exception
import System.Exit (ExitCode(..))

tunnelStatus :: Tunnel -> IO TunnelStatus
tunnelStatus tunnel = atomically $ readTVar (status tunnel)

killTunnel ::  Tunnel -> IO ()
killTunnel tunnel = do
    threadId <- atomically $ do
        tStatus <- readTVar (status tunnel)
        case tStatus of
            Active tId -> pure $  Just tId
            _ -> pure Nothing
    case threadId of
        Just id -> throwTo id ThreadKilled
        Nothing -> pure ()

restartTunnel :: BChan AppEvent -> Tunnel -> IO ()
restartTunnel bChan tunnel = do
    tStatus <- atomically $ readTVar (status tunnel)
    case tStatus of
        Active id -> throwTo id RestartRequested
        Starting -> pure ()
        Restarting -> pure ()
        _ -> atomically $ writeTVar (status tunnel) Restarting
    writeBChan bChan Refresh

startTunnel :: BChan AppEvent -> Tunnel -> IO ()
startTunnel bChan tunnel = do
    running <- atomically $ do
        tStatus <- readTVar (status tunnel)
        case tStatus of
            Starting -> pure True
            Active _ -> pure True
            _ -> do
                writeTVar (status tunnel) Starting
                pure False
    unless running $ do
        startMsg <- toLogMsg tunnel "Starting..."
        writeBChan bChan startMsg
        threadId <- forkTunnel bChan tunnel
        atomically $ do
            tStatus <- readTVar (status tunnel)
            case tStatus of
                Starting -> writeTVar (status tunnel) (Active threadId)
                _ -> pure ()
        writeBChan bChan Refresh


forkTunnel :: BChan AppEvent ->  Tunnel -> IO ThreadId
forkTunnel bChan tunnel = forkIO $ runTunnel `catches` handleExceptions `finally` writeBChan bChan Refresh

  where
    runTunnel = do
        let cp = PS.shell . toCommand $ tunnel
        (code, sOut, sErr) <- PS.readCreateProcessWithExitCode cp ""
        unless (null sOut) $ handleMsg sOut
        unless (null sErr) $ handleMsg sErr
        case code of
            ExitFailure i -> handleMsg $ "Process exited with code " ++ show i
            _ -> handleMsg $ "Process exited"
        setStatus Restarting 

    handleMsg text = do
        msg <- toLogMsg tunnel text
        writeBChan bChan msg

    handleExceptions = [
          Handler (\ (ex :: AsyncException ) -> setStatus Killed >>  handleMsg "Killed")
        , Handler (\ (ex :: AppException) -> setStatus Restarting >> handleMsg "Restart requested") 
        , Handler (\ (ex :: SomeException) -> setStatus Restarting >> handleMsg ("Unknown exception: " ++ (show ex))) 
        ]

    setStatus newStatus = atomically $ writeTVar (status tunnel) newStatus

toLogMsg :: Tunnel -> String -> IO AppEvent
toLogMsg tunnel line = do
    localTime <- getZonedTime
    let time = formatTime defaultTimeLocale "%T" localTime
    pure $ LogMsg $ Msg (name tunnel) time line

toCommand :: Tunnel -> String
toCommand tunnel = concat [ 
      prefix
    , portForwardFlag
    , remoteAddress
    , doNotExecuteFlag
    , options
    ]
  where
    prefix = "ssh " ++ (user tunnel) ++ "@" ++ (tunnelThrough tunnel)
    portForwardFlag = " -L "
    remoteAddress = (show $ localPort tunnel) ++ ":" ++ (remoteHost tunnel) ++ ":" ++ (show $ remotePort tunnel)
    doNotExecuteFlag = " -N "
    options = "-o ExitOnForwardFailure=True -o ConnectTimeout=3"


