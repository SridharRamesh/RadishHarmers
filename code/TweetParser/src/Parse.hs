{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables #-}
module Parse where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.UTF8 as StrictUTF8
import qualified Data.ByteString.Lazy as ByteString
import Data.Attoparsec.ByteString.Lazy hiding (string)
import qualified Data.Attoparsec.ByteString.Lazy
import GHC.Generics
import Data.Text
import Data.List
import Data.Maybe
import Data.String

literal = Data.Attoparsec.ByteString.Lazy.string

preludeParser = literal "window.YTD.tweet.part0 = "
fileParser = preludeParser >> json
fileParse = parse fileParser

failText text = fail (unpack text)

monthNames :: IsString s => [s]
monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

stringToMonth string = elemIndex string monthNames

stringToGuaranteedMonth string = 
  fromMaybe 
    (error $ "This should be unreachable, but something went awry parsing month: " <> StrictUTF8.toString string) 
    (stringToMonth string)

parseNatural = error "Unimplemented"

data Timestamp = Timestamp {
  year :: Int,
  month :: Int,
  dayOfMonth :: Int, -- NOT the day of the week
  hour :: Int,
  minute :: Int,
  second :: Int
} deriving (Show)

dayOfWeekNames = ["Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat"]

parseSpace = literal " "
parseColon = literal ":"
parseTimeZone = literal "+0000"
parseDayOfWeek = choice [literal dayOfWeek | dayOfWeek <- dayOfWeekNames]
parseMonth = choice [literal month | month <- monthNames]

parseTimestamp = do
  parseDayOfWeek -- We ignore the result
  parseSpace
  monthAsString <- parseMonth
  let (month :: Int) = stringToGuaranteedMonth monthAsString
  dayOfMonth <- parseNatural
  parseSpace
  hour <- parseNatural
  parseColon
  minute <- parseNatural
  parseColon
  second <- parseNatural
  parseSpace
  parseTimeZone
  parseSpace
  year <- parseNatural
  return Timestamp{..}

data Tweet = Tweet {
  id :: Text,
  full_text  :: Text,
  created_at :: Text,
  favorite_count :: Text,
  retweet_count :: Text,
  in_reply_to_status_id :: Maybe Text,
  in_reply_to_user_id :: Maybe Text,
  in_reply_to_screen_name :: Maybe Text
} deriving (Show)
$(deriveFromJSON defaultOptions ''Tweet)

data BoxedTweet = BoxedTweet {
  tweet :: Tweet
} deriving (Show)
$(deriveFromJSON defaultOptions{rejectUnknownFields = True} ''BoxedTweet)

newtype FileTop = FileTop [BoxedTweet]
$(deriveFromJSON defaultOptions ''FileTop)