{-# LANGUAGE OverloadedStrings #-}

module Ui.Attrs where

import Brick.AttrMap
import Brick.Util
import qualified Graphics.Vty.Attributes as Vty

statusActive :: AttrName
statusActive = "status" <> "active"

statusWarning :: AttrName
statusWarning = "status" <> "warning"

statusError :: AttrName
statusError = "status" <> "error"


attrs :: AttrMap
attrs = attrMap Vty.defAttr [
      (statusActive, fg Vty.green)
    , (statusWarning, fg Vty.yellow)
    , (statusError, fg Vty.red)
    ]