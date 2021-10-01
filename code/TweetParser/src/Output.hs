{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Output(makePage) where

import Prelude hiding (unlines, show)
import qualified Prelude
import Parse
import Data.Text.Lazy
import qualified Data.String

show x = Data.String.fromString $ Prelude.show x

dateToText :: Date -> Text
dateToText Date{..} = (show year) <> "-" <> (show month) <> "-" <> (show dayOfMonth)

makeTweet :: Tweet -> Text
makeTweet Tweet{..} = unlines
  ["Id: " <> fromStrict id,
   "Text: " <> fromStrict full_text,
   "Created at: " <> dateToText (date created_at),
   "----"
  ]

pageHeader = unlines 
  ["<html>", 
   "<head>",
   "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">"
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