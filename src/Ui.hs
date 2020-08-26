module Ui where

import Config (Config)
import qualified Config as CFG
import Data.Word (Word16)
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Attributes as Vty
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TVar
import Brick
import Brick.BChan
-- import Brick.Main
import Brick.Widgets.List
import qualified Brick.Types as T
import qualified Data.Vector as V
import Ui.Model
import Ui.Widgets (drawUI)
import Ui.Attrs (attrs)
import Tunnel (startTunnel, restartTunnel, killTunnel, tunnelStatus)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, forM, forM_)


runUI :: Config -> IO AppState
runUI cfg = do
    appState <- toAppState  cfg
    bChan <- newBChan 10000
    let app = mkApp appState bChan
        buildVty = Vty.mkVty Vty.defaultConfig
    initialVty <- Vty.mkVty Vty.defaultConfig
    customMain initialVty buildVty (Just bChan) app appState

            

eventHandler :: BChan AppEvent -> AppState -> BrickEvent AppResource AppEvent -> EventM AppResource (Next AppState)
eventHandler bChan state (T.VtyEvent e) =
    case e of
      Vty.EvKey Vty.KEsc _ -> do
        let state' = state {active = False}
        liftIO $ forM_ (tunnels state) killTunnel
        liftIO $ writeBChan bChan Refresh
        continue state'


      Vty.EvKey Vty.KEnter _ -> do
        let t = snd <$> listSelectedElement (tunnels state)
        case t of
          Just tunnel -> liftIO $ startTunnel bChan tunnel
          Nothing -> pure ()
        continue state
      
      Vty.EvKey (Vty.KChar 'a') _ -> do
        let t = snd <$> listSelectedElement (tunnels state)
        case t of
          Just tunnel -> liftIO $ startTunnel bChan tunnel
          Nothing -> pure ()
        continue state
      
      Vty.EvKey (Vty.KChar 'A') _ -> do
        liftIO $ forM_ (tunnels state) (startTunnel bChan)
        continue state
      
      Vty.EvKey (Vty.KChar 'k') _ -> do
        let t = snd <$> listSelectedElement (tunnels state)
        case t of
          Just tunnel -> liftIO $ killTunnel tunnel
          Nothing -> pure ()
        continue state
      
      Vty.EvKey (Vty.KChar 'K') _ -> do
        liftIO $ forM_ (tunnels state) killTunnel
        continue state

      Vty.EvKey (Vty.KChar 'r') _ -> do
        let t = snd <$> listSelectedElement (tunnels state)
        case t of
          Just tunnel -> liftIO $ restartTunnel bChan tunnel
          Nothing -> pure ()
        continue state

      Vty.EvKey (Vty.KChar 'R') _ -> do
        liftIO $ forM_ (tunnels state) (restartTunnel bChan)
        continue state

      Vty.EvKey (Vty.KChar 'C') _ -> do
        let logs' = listClear (logs state)
        continue $ state {logs = logs'}

      _ -> do 
        tunnels' <- handleListEvent e (tunnels state)
        let state' = state {tunnels = tunnels'}
        continue state'


eventHandler bChan state (AppEvent Refresh) = do
  tunnels' <- forM (tunnels state) $ \t -> do
    status' <- liftIO $ tunnelStatus t
    pure $ t {uStatus = status'}
  let running = active state
      anyTunnelActive = any (isActive . uStatus) tunnels'
      state' = state {tunnels = tunnels'}
  
  forM_ tunnels' $ \t -> do
    let isRestarting = uStatus t == Restarting
    when isRestarting $ liftIO $ startTunnel bChan t 

  if running || anyTunnelActive
    then continue state'
    else halt state'
    
  where
    isActive (Active _) = True
    isActive Starting = True
    isActive Restarting = True
    isActive _ = False

eventHandler _ state (AppEvent (LogMsg msg)) = do
  let size = length . listElements . logs $ state
      logs' = listMoveTo (size+1) $ listInsert size msg (logs state)
  continue $ state {logs = logs'}
eventHandler _ state _ = continue  state


mkApp :: AppState -> BChan AppEvent -> App AppState AppEvent AppResource
mkApp state bChan = App {
    appDraw  = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = eventHandler bChan
  , appStartEvent = pure
  , appAttrMap = const attrs

}