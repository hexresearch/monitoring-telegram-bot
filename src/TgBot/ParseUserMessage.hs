module TgBot.ParseUserMessage
    ( parseTgCommand
    , TgCommand(..)
    ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.String

data TgCommand = TgCommandStats | TgCommandHelp

parseTgCommand :: Text -> Maybe TgCommand
parseTgCommand = either (const Nothing) Just . runParser command "" . T.unpack
  where
    command = stats <|> help

    stats :: Parsec Dec String TgCommand
    stats = do
        try $ string "/stats"
        return TgCommandStats

    help :: Parsec Dec String TgCommand
    help = do
        try $ string "/help" <|> string "/start"
        return TgCommandHelp
