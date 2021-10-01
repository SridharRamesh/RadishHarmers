{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeApplications, RecordWildCards #-}
module Main where

import Constants
import Parse
import Output
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy as ByteString
import Data.Attoparsec.ByteString.Lazy
import Data.List (sortBy, groupBy)
import Control.Monad (when)
import Data.Function (on)
import System.Directory
import System.FilePath

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
        Success (FileTop boxedTweets) -> 
          let orderedTweets = sortBy (compare `on` created_at) (map tweet boxedTweets)
              groupedTweets = groupBy ((==) `on` (date . created_at)) orderedTweets
              datesAndTweets = [(date $ created_at $ head tweets, tweets) | tweets <- groupedTweets]
              datesAndHTML = [(date, makePage date tweets) | (date, tweets) <- datesAndTweets]
          in do mapM (uncurry writePage) datesAndHTML
                putStrLn $ "All done!"

writePage Date{..} page = do
  outputPath <- canonicalizePath $ outputDirectory </> (show year) </> (show month) </> (show dayOfMonth) <> ".html"
  -- We canonicalize the name just for more readable terminal messages in the next line
  putStrLn $ "Writing output to: " <> outputPath
  createAndWriteFile outputPath page

createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  ByteString.writeFile path content