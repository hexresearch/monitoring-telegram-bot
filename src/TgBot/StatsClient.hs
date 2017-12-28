{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module TgBot.StatsClient
    ( queryStats
    , Stats
    ) where

import           Data.Aeson
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Text.InterpolatedString.Perl6 (qc)
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Servant.API
import Servant.Client

data StatInt = StatInt
    { val :: !Int
    } deriving Generic

instance FromJSON StatInt

instance Show StatInt where
    show (StatInt v) = show v

data Stats = Stats
    { err503_count  :: !StatInt
    , session_count :: !StatInt
    , request_count :: !StatInt
    , active_nodes  :: !StatInt
    } deriving Generic

instance FromJSON Stats

instance Show Stats where
    show Stats{..} = [qc|Active nodes: {active_nodes}
Sessions: {session_count}
Requests: {request_count}
Error 503: {err503_count}|]

stats :: ClientM Stats

type API = Get '[JSON] Stats

api :: Proxy API
api = Proxy

stats = client api

queryStats :: Text -> IO (Either ServantError Stats)
queryStats url = do
    burl <- parseBaseUrl $ T.unpack url
    manager <- newManager defaultManagerSettings
    runClientM stats (ClientEnv manager burl)
