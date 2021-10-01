{-# LANGUAGE OverloadedStrings #-}
module Output where

import Parse (Tweet, Timestamp, Date)
import Data.String

makePage date tweets = "This is the page for date: " <> (fromString (show date)) <> " with tweets: " <> (fromString (show tweets))