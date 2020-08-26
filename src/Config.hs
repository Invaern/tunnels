{-# LANGUAGE DeriveGeneric #-}
module Config where

import GHC.Generics
import Data.Word (Word16)

import Data.Yaml

data Config = Config {
     tunnels :: [TunnelConf]
 } deriving (Generic, Show)

data TunnelConf = TConf {
    name :: String 
  , user :: String  
  , tunnelThrough :: String
  , localPort :: Word16
  , remotePort :: Word16
  , remoteHost :: String
} deriving (Generic, Show)

instance FromJSON TunnelConf
instance FromJSON Config

decodeConfig :: FilePath -> IO (Either ParseException Config)
decodeConfig filePath = decodeFileEither filePath