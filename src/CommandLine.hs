module CommandLine (Args(..), parseArgs) where

import Options.Applicative
import Data.Semigroup ((<>))

newtype Args = Args {
    configPath :: FilePath
}

argsParser :: Parser Args
argsParser = Args 
    <$> strArgument (metavar "CFG" <> help "Config file location")

parseArgs :: IO Args
parseArgs = execParser opts
  where
      opts = info (argsParser <**> helper)
        (fullDesc 
        <> progDesc "SSH tunnel manager")
