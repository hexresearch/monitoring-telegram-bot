module TgBot.Server.Config (
    BotConfig(..)
  , BotSchedule
  , readConfig
  ) where

import Data.Aeson.Types
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.LocalTime (TimeOfDay)
import Data.Yaml
import Data.Yaml.Config
--import GHC.Generics

data BotConfig = BotConfig {
    botToken    :: !Text
  , botChannel  :: !Int64 -- we don't have names for private channels: use id
  , botSchedule :: !BotSchedule
  , botStatsUrl :: !Text
}-- deriving Generic

type BotSchedule = [TimeOfDay]

instance FromJSON BotConfig where
    parseJSON (Object o) = BotConfig
        <$> o .: "token"
        <*> o .: "channel"
        <*> o .: "time"
        <*> o .: "stats_server"
    parseJSON wat = typeMismatch "BotConfig" wat

readConfig :: FilePath -> IO BotConfig
readConfig f = loadYamlSettings [f] [] useEnv
