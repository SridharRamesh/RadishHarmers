{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Output(makePage) where

import Prelude hiding (unlines, show, id)
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
-- Actually, should probably switch to blaze HTML or some such library/

show x = Data.String.fromString $ Prelude.show x

lineBreak = "<br>\n"
paragraph Nothing x = "<p>\n" <> x <> "\n</p>\n"
paragraph (Just anchor) x = "<p id=\"" <> anchor <> "\">\n" <> x <> "\n</p>\n"
horizontalRule = "<hr>\n"

dateToText :: Date -> Text
dateToText Date{..} = (show year) <> "-" <> (show month) <> "-" <> (show dayOfMonth)

dateToURL :: Date -> Text
dateToURL Date{..} = (show year) <> "/" <> (show month) <> "/" <> (show dayOfMonth) <> ".html"

timestampToText :: Timestamp -> Text
timestampToText Timestamp{..} = 
    (show year) <> "-" <> (show month) <> "-" <> (show dayOfMonth)
    <> " at "
    <> (show hour) <> ":" <> (show minute) <> ":" <> (show second)

-- Taken from https://developer.twitter.com/en/docs/twitter-for-websites/embedded-tweets/guides/css-for-embedded-tweets
-- Along with the corresponding CSS stylesheet.
blockquoteStart = "<blockquote class=\"twitter-tweet\" data-lang=\"en\">\n<p lang=\"en\" dir=\"ltr\">\n"
blockquoteEnd = "</p>\n</blockquote>\n"

-- Taken and adapted from HTMLEntities-Builder
htmlSanitizeChar c =
  fromMaybe (Text.Builder.singleton c) $
  lookup c htmlCharMap

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

concatenateWithLineBreaks = intercalate lineBreak

linkify text url = "<a href=\"" <> url <> "\">" <> text <> "</a>"

separate text = text <> "\n"

icon altText imageURL width height = "<img class=\"icon\" src=\"" <> imageURL <> "\" alt=\"" <> altText <> "\" width=\"" <> (show width) <> "\" height=\"" <> (show height) <> "\">\n"
avi altText imageURL width height = "<img class=\"avi\" src=\"" <> imageURL <> "\" alt=\"" <> altText <> "\" width=\"" <> (show width) <> "\" height=\"" <> (show height) <> "\">\n"



makeTweet :: Tweet -> Text
makeTweet tweet@Tweet{..} = separate $ paragraph (Just $ fromStrict id) $ concatenateWithLineBreaks elements
  where 
    elements = basicElements ++ replyElements
    basicElements = 
      [horizontalRule <> blockquoteStart <> tweetHeader <> lineBreak <> (fromStrict full_text) <> lineBreak <> lineBreak <> tweetFooter <> blockquoteEnd,
       "Id: " <> (linkify (fromStrict id) (tweetToURL tweet))
      ]
    tweetHeader = separate $ (avi "avi" aviIcon 50 50) <> "<b>" <> displayName <> "</b> <span class=\"display-name\" title=\"" <> (timestampToText created_at) <> "\"> @RadishHarmers · " <> (dateToText $ date created_at) <> "</span>"
    tweetFooter = retweetDisplay <> " &nbsp; &nbsp; &nbsp; " <> faveDisplay
    retweetDisplay = (icon "Retweets" retweetIcon 20 20) <> (fromStrict retweet_count)
    faveDisplay = (icon "Likes" likeIcon 20 20) <> (fromStrict favorite_count)
    replyElements = case in_reply_to_status_id of
      Nothing -> []
      (Just replied_to_id) -> ["In reply to ID: " <> (fromStrict replied_to_id)]

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
  mconcat [makeTweet tweet | tweet <- tweets, not (isSelfRT tweet), not (isRT tweet)]
  <>
  pageFooter

isSelfRT tweet = (Data.String.fromString ("RT @" <> atName) :: StrictText.Text) `StrictText.isPrefixOf` (full_text tweet)
isRT tweet = (Data.String.fromString ("RT @") :: StrictText.Text) `StrictText.isPrefixOf` (full_text tweet)
isReply tweet = case in_reply_to_status_id tweet of Nothing -> False; Just _ -> True
-- Should detect self-replies; i.e., threads. Should also perhaps detect forward on threads.
containsMultiSpace tweet = (Data.String.fromString ("  ") :: StrictText.Text) `StrictText.isInfixOf` (full_text tweet)

baseURL = "../../"
iconFolder = baseURL <> "icons/"
likeIcon = iconFolder <> "Like.png"
retweetIcon = iconFolder <> "Retweet.png"
aviIcon = iconFolder <> "Avi.jpg"
banner = iconFolder <> "Banner.jpg"

-- TODO: Linkify other people's tweets back to twitter.com.
tweetToURL tweet = baseURL <> (dateToURL $ date $ created_at tweet) <> "#" <> (fromStrict $ id tweet)