{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Output(makePage) where

import Prelude hiding (unlines, show)
import Constants
import qualified Prelude
import Parse
import Data.Text.Lazy as Text
import qualified Data.Text as StrictText
import qualified Data.String
import Data.Text.Lazy.Builder as Text.Builder
import Data.Maybe

-- Should probably be doing everything in Builders and converting to Text just at the end.
-- Such a mess figuring out these different formats and what to use.

show x = Data.String.fromString $ Prelude.show x

lineBreak = "\n<br>\n"
paragraph Nothing x = "<p>\n" <> x <> "\n</p>\n"
paragraph (Just anchor) x = "<p id=\"" <> anchor <> "\">\n" <> x <> "\n</p>\n"
horizontalRule = "\n<hr>\n"

dateToText :: Date -> Text
dateToText Date{..} = (show year) <> "-" <> (show month) <> "-" <> (show dayOfMonth)

timestampToText :: Timestamp -> Text
timestampToText Timestamp{..} = 
    (show year) <> "-" <> (show month) <> "-" <> (show dayOfMonth)
    <> " "
    <> (show hour) <> ":" <> (show minute) <> ":" <> (show second)

-- Taken from https://developer.twitter.com/en/docs/twitter-for-websites/embedded-tweets/guides/css-for-embedded-tweets
-- Along with the corresponding CSS stylesheet.
blockquoteStart = "<blockquote class=\"twitter-tweet\" data-lang=\"en\"><p lang=\"en\" dir=\"ltr\">"
blockquoteEnd = "</p></blockquote>"

-- Taken and adapted from HTMLEntities-Builder
--htmlSanitizeChar :: Char -> Text.Builder
htmlSanitizeChar c =
  fromMaybe (Text.Builder.singleton c) $
  lookup c htmlCharMap

--htmlCharMap :: [(Char, Text.Builder)]
htmlCharMap =
  [
    ('<', "&lt;"),
    ('>', "&gt;"),
    ('&', "&amp;"),
    ('"', "&quot;"),
    ('\'', "&#39;"),
    ('\n', "<br>")
  ]

htmlSanitize text = toLazyText $ StrictText.foldr (\c b -> htmlSanitizeChar c <> b) mempty text

makeTweet :: Tweet -> Text
makeTweet Tweet{..} = paragraph (Just $ fromStrict id) $ intercalate lineBreak
  [blockquoteStart <> htmlSanitize full_text <> blockquoteEnd,
   "Timestamp: " <> timestampToText created_at,
   "Id: " <> fromStrict id,
   horizontalRule
  ]

pageHeader = unlines 
  ["<html>", 
   "<head>",
   "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">",
   "<!-- This page was generated from TweetParser. -->",
   "<link rel=\"stylesheet\" href=\"../../styles.css\">",
   "</head>",
   "<body>"
  ]

pageFooter = unlines
  ["</body>",
   "</html>"
  ]

makePage :: Date -> [Tweet] -> Text
makePage date tweets = 
  pageHeader
  <>
  "These are all the tweets and replies that I made on "
  <>
  dateToText date
  <>
  horizontalRule
  <>
  mconcat [makeTweet tweet | tweet <- tweets, not (isSelfRT tweet), not (isRT tweet)]
  <>
  pageFooter

isSelfRT tweet = (Data.String.fromString ("RT @" <> atName) :: StrictText.Text) `StrictText.isPrefixOf` (full_text tweet)
isRT tweet = (Data.String.fromString ("RT @") :: StrictText.Text) `StrictText.isPrefixOf` (full_text tweet)
isReply tweet = case in_reply_to_status_id tweet of Nothing -> False; Just _ -> True
-- Should detect self-replies; i.e., threads. Should also perhaps detect forward on threads.
containsMultiSpace tweet = (Data.String.fromString ("  ") :: StrictText.Text) `StrictText.isInfixOf` (full_text tweet)