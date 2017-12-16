module TgBot.Server.Bot (
    runStatsBot
    ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Int (Int64)
import           Data.Time
import           Data.Monoid ((<>))
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.AlarmClock

import           Network.HTTP.Client      (newManager, Manager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot

import           Text.InterpolatedString.Perl6 (qc)

import           TgBot.Server.Config (BotConfig(..))
import           TgBot.ParseUserMessage
import           TgBot.StatsClient

runStatsBot :: BotConfig -> IO ()
runStatsBot cfg = do
    let token = Token $ "bot" <> botToken cfg
    manager <- newManager tlsManagerSettings
    dumpStatsOnAlarm (botStatsUrl cfg) token manager (botChannel cfg) (botSchedule cfg)
    runReplyStats (botStatsUrl cfg) token manager

dumpStatsOnAlarm :: Text -> Token -> Manager -> Maybe Int64 -> Maybe TimeOfDay -> IO ()
dumpStatsOnAlarm url token manager (Just channel) (Just tod) = do
    time <- getNextAlarm tod
    alrm <- newAlarmClock (sendStatsToChannel url token manager channel tod)
    setAlarm alrm time
dumpStatsOnAlarm _ _ _ _ _ = print [qc|No stats channel setting in config|]

sendStatsToChannel :: Text -> Token -> Manager -> Int64 -> TimeOfDay
    -> AlarmClock UTCTime -> IO ()
sendStatsToChannel url token manager channel tod _ = do
    time <- getNextAlarm tod
    alrm <- newAlarmClock (sendStatsToChannel url token manager channel tod)
    setAlarm alrm time
    sendStatsReply url reply'
  where
    reply' = reply token manager channel Nothing

getNextAlarm :: TimeOfDay -> IO (UTCTime)
getNextAlarm tod = do
    now <- getZonedTime
    let lt = zonedTimeToLocalTime now
        lt' = if localTimeOfDay lt < tod
                 then lt{localTimeOfDay = tod}
                 else LocalTime (succ (localDay lt)) tod
    let new = zonedTimeToUTC $ now{zonedTimeToLocalTime = lt'}
    print [qc|Next Alarm is {new}|]
    return new

runReplyStats :: Text -> Token -> Manager -> IO ()
runReplyStats url token manager = loop Nothing
  where
    loop moffset = do
      let client = getUpdatesM $ GetUpdatesRequest
              { updates_offset = moffset
              , updates_limit = Nothing
              , updates_timeout = Just 5  -- ^ seconds
              , updates_allowed_updates = Just ["message"]
              }
      upd <- runClient client token manager
      case upd of
          Left servantError -> do
              print [qc| Querying telegram api error: {servantError}|]
              threadDelay $ 5 * 1000000
          Right resp -> do
              let updates = result resp
              mapM_ (processUserRequest url token manager) updates

              loop $ case updates of
                  [] -> moffset
                  us -> Just $ (maximum [update_id u | u <- us]) + 1

processUserRequest :: Text -> Token -> Manager -> Update -> IO ()
processUserRequest statsUrl token manager upd = do
    flip (maybe (return ())) ((message >=> text) upd) $ \msg -> do
        case parseTgCommand msg of
            Nothing ->
                  print [qc|Invalid user command: {msg}|]
            Just TgCommandStats ->
                  sendStatsReply statsUrl reply'
            Just TgCommandHelp ->
                  sendHelpReply reply'
  where
    reply' = maybe (const $ return ())
               (\m -> reply token manager (chat_id $ chat m) (Just $ message_id m))
               $ message upd

sendStatsReply :: Text -> (Text -> IO ()) -> IO ()
sendStatsReply statsUrl reply' = do
    r <- queryStats statsUrl
    case r of
        Left err -> do
            print [qc|Error while querying stats: {err}|]
            reply' $ "Error while querying stats"
        Right stats ->
            reply' $ T.pack $ show stats

sendHelpReply :: (Text -> IO ()) -> IO ()
sendHelpReply reply' = do
    let help = [qc|Stats: `/stats`
    Dump hexiperf statistics

Help: `/help`
    Show this message|]
    reply' help

reply :: Token -> Manager -> Int64 -> Maybe Int -> Text -> IO ()
reply token manager chatId msgId txt = do
    let client = sendMessageM $
            (sendMessageRequest (ChatId chatId) txt)
            { message_reply_to_message_id = msgId
            , message_parse_mode = Just Markdown
            , message_disable_web_page_preview = Just True
            }
    res <- runClient client token manager
    case res of
        Left servantError -> do
            print [qc| Send message error: {servantError}|]
            threadDelay $ 5 * 1000000
        Right resp ->
            return ()
