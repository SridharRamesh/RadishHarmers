{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy as ByteString
import Data.Attoparsec.ByteString.Lazy

preludeParser = string "window.YTD.tweet.part0 = "
fileParser = preludeParser >> json
fileParse = parse fileParser