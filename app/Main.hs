module Main where

import           Data.Monoid
import           Options.Applicative

import           TgBot.Server.Bot (runStatsBot)
import           TgBot.Server.Config (readConfig)

-- | Argument line options
data Options = Options {
  -- | Path to config, if not set, the app will not start
  configPath :: FilePath
}

-- | Command line parser
optionsParser :: Parser Options
optionsParser = Options
  <$> strOption (
         long "conf"
      <> metavar "CONFIG"
      <> help "Path to configuration file"
    )

-- | Execute server with given options
runBot :: Options -> IO ()
runBot Options{..} = do
  cfg <- readConfig configPath
  runStatsBot cfg

main :: IO ()
main = execParser opts >>= runBot
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Telegram bot for retreiving hexiperf stats"
     <> header "hexiperf-atas-telegram-bot - dumps hexiperf stats" )
