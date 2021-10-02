{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Output(makePage) where

import Prelude hiding (unlines, show)
import qualified Prelude
import Parse
import Data.Text.Lazy as Text
import qualified Data.Text as StrictText
import qualified Data.String
import Data.Text.Lazy.Builder as Text.Builder
import Data.Maybe

-- Should probably be doing everything in builder's and converting to Text just at the end.
-- Such a mess figuring out these different formats and what to use.

show x = Data.String.fromString $ Prelude.show x

lineBreak = "\n<br>\n"
paragraph x = "<p>\n" <> x <> "\n</p>\n"
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
makeTweet Tweet{..} = paragraph $ intercalate lineBreak
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
  "These are all the tweets and RTs that I made on "
  <>
  dateToText date
  <>
  horizontalRule
  <>
  mconcat [makeTweet tweet | tweet <- tweets]
  <>
  pageFooter