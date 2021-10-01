{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Output(makePage) where

import Prelude hiding (unlines, show)
import qualified Prelude
import Parse
import Data.Text.Lazy
import qualified Data.String
import HTMLEntities.Builder
import Data.Text.Lazy.Builder

show x = Data.String.fromString $ Prelude.show x

lineBreak = "\n<br>\n"

paragraph x = "<p>\n" <> x <> "\n</p>\n"

dateToText :: Date -> Text
dateToText Date{..} = (show year) <> "-" <> (show month) <> "-" <> (show dayOfMonth)

timestampToText :: Timestamp -> Text
timestampToText Timestamp{..} = 
    (show year) <> "-" <> (show month) <> "-" <> (show dayOfMonth)
    <> " "
    <> (show hour) <> ":" <> (show minute) <> ":" <> (show second)

makeTweet :: Tweet -> Text
makeTweet Tweet{..} = paragraph $ intercalate lineBreak
  ["<pre>" <> (toLazyText $ HTMLEntities.Builder.text full_text) <> "</pre>",
   "Timestamp: " <> timestampToText created_at,
   "Id: " <> fromStrict id,
   "<hr>"
  ]

pageHeader = unlines 
  ["<html>", 
   "<head>",
   "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">",
   "<!-- This page was generated from TweetParser. -->"
  ]

pageFooter = unlines
  ["</head>",
   "</html>"
  ]

makePage :: Date -> [Tweet] -> Text
makePage date tweets = 
  pageHeader
  <>
  "This is the page for date: "
  <>
  dateToText date
  <>
  " with tweets: \n"
  <>
  mconcat [makeTweet tweet | tweet <- tweets]
  <>
  pageFooter