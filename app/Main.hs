module Main where

import CommandLine (parseArgs, Args(..))
import Data.Functor (($>))
import Config
import Ui (runUI)

main :: IO ()
main = do
    args <- parseArgs
    res <- decodeConfig (configPath args)
    case res of
        Left e -> print e
        Right cfg -> runUI cfg $> ()
