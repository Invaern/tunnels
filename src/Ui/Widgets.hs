module Ui.Widgets (drawUI) where 

import Ui.Model
import Ui.Attrs
import Brick
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Center

drawUI :: AppState -> [Widget AppResource]
drawUI s = [(tunnelsList <+> logsList) <=> legend]
  where
      tunnelsList = border $ hLimit 72 $  renderList drawTunnel True (tunnels s)
      logsList = border $ renderList (\_ -> drawLog) False (logs s)

drawLog :: Msg -> Widget AppResource
drawLog msg = hBox [
    uiName
  , uiTime
  , uiLine
  ] 
  where
    uiName = hLimit 15 $ padLeft (Pad 1) $ padRight Max $ str $ mName msg
    uiTime = hLimit 12 $ hCenter $ str $ wrap '[' ']' (mTime msg)
    uiLine = strWrap $ mLine msg
    wrap l r msg = l : msg ++ [r]

drawTunnel :: Bool -> Tunnel -> Widget AppResource
drawTunnel selected tunnel = hBox [
    indicator
  , label
  , port
  , remote
  , statusUi
  ]
  where
    indicator = hLimit 3 $ if selected then str " * " else str "   "
    label = hLimit 20 $ padRight Max $ str (name tunnel)
    port = hLimit 10 $ padRight Max $ str $ "(" ++ show (localPort tunnel) ++ ")"
    remote = hLimit 25 $ padRight Max $ str $ remoteHost tunnel ++ ":" ++ show (remotePort tunnel)
    statusUi = str "[" <+> (hLimit 12 $  hCenter statusText) <+> str "]"
      where
        statusText =  case (uStatus tunnel) of
          Inactive -> withAttr statusWarning $ str "Inactive"
          Killed -> withAttr statusError $ str "Killed"
          Restarting -> withAttr statusWarning $ str "Restarting"
          Active _ -> withAttr statusActive $ str "Active"


legend :: Widget AppResource
legend = border $ padRight Max $ hBox [
    activate
  , kill
  , restart
  , clearLogs <=> exit
  ]
  where
    activate = padLeft (Pad 1) $ str "a: Activate selected" <=> str "A: Active all"
    kill = padLeft (Pad 5) $  str "k: Kill selected" <=> str "K: Kill all"
    restart = padLeft (Pad 5) $ str "r: Restart selected" <=> str "R: Restart all"
    clearLogs = padLeft (Pad 5) $ str "C: Clear all logs"
    exit = padLeft (Pad 5) $ str "ESC: Exit"