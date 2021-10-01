{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables #-}
module Parse where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Attoparsec.ByteString.Lazy as LazyStringParse
import Data.Attoparsec.Text
import GHC.Generics
import Data.Text
import Data.List
import Data.Maybe
import Data.String

-- Note: The argument to literal needs to be a STRICT ByteString, not a lazy one.
literal x = 
  do string x -- From Data.Attoparsec
     return () -- Do not expect to recover the string from this function

-- Note: The first argument to literalWithReturn needs to be a STRICT ByteString, not a lazy one.
-- The second argument needs to be a lazy string.
literalWithReturn strictMatch lazyReturn =
  do string strictMatch -- From Data.Attoparsec
     return lazyReturn

preludeParser = LazyStringParse.string "window.YTD.tweet.part0 = "
fileParser = preludeParser >> json
fileParse = LazyStringParse.parse fileParser

failText text = fail (unpack text)

monthNames :: IsString s => [s]
monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

stringToMonth string = elemIndex string monthNames

stringToGuaranteedMonth string = 
  fromMaybe 
    (error $ "This should be unreachable, but something went awry parsing month: " <> unpack string) 
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
parseMonth = choice [literalWithReturn strictMonthName lazyMonthName | (strictMonthName, lazyMonthName) <- Data.List.zip monthNames monthNames]

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

textParserToJSONParser typeName textParser = withText typeName $ \text ->
  case parse textParser text of
    (Fail _ _ _) -> fail "Died"
    (Done _ r) -> return r

instance FromJSON Timestamp where
  parseJSON = textParserToJSONParser "Timestamp" parseTimestamp


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