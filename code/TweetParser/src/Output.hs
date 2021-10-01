{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Output(makePage) where

import Prelude hiding (unlines, show)
import qualified Prelude
import Parse
import Data.Text.Lazy
import qualified Data.String

show x = Data.String.fromString $ Prelude.show x

lineBreak = "\n<br>\n"

paragraph x = "<p>\n" <> x <> "\n</p>\n"

dateToText :: Date -> Text
dateToText Date{..} = (show year) <> "-" <> (show month) <> "-" <> (show dayOfMonth)

makeTweet :: Tweet -> Text
makeTweet Tweet{..} = paragraph $ intercalate lineBreak
  ["Id: " <> fromStrict id,
   "Text: " <> fromStrict full_text,
   "Created at: " <> dateToText (date created_at),
   "----"
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