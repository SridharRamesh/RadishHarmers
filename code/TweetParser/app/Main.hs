{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy as ByteString
import Data.Attoparsec.ByteString.Lazy

inputPath = "../../raw/TweetRecent.js"

preludeParser = string "window.YTD.tweet.part0 = "

main :: IO ()
main = do
  file <- ByteString.readFile inputPath
  putStrLn $ show $ ByteString.length file
  let x = parse json file
  case x of
    Done _ _ -> putStrLn "Success"
    Fail i x z -> putStrLn $ UTF8.toString $ ByteString.take 10 i