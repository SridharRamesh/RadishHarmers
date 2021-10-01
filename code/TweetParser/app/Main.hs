{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Constants
import Parse
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy as ByteString
import Data.Attoparsec.ByteString.Lazy
import Control.Monad (when)

main :: IO ()
main = do
  file <- ByteString.readFile inputPath
  case fileParse file of
    Fail i x z -> do
      putStrLn $ "Failure parsing file " ++ inputPath ++ " as JSON"
      putStrLn "Here are the first 10 characters of the failure point:"
      putStrLn $ UTF8.toString $ ByteString.take 10 i
    Done leftover result -> do
      when (ByteString.length leftover > 0) $ 
           putStrLn ("JSON file had leftover bit: " ++ UTF8.toString leftover)
      let tweets = fromJSON result
      case tweets of
        Error errorString -> putStrLn $ "Tweets parse error: " ++ errorString
        Success (FileTop list) -> putStrLn $ show $ Prelude.take 10 $ list