{-# LANGUAGE 
  OverloadedStrings, 
  ScopedTypeVariables, 
  TypeApplications, 
  RecordWildCards, 
  ViewPatterns 
#-}
module Main where

import Prelude hiding (writeFile)
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
import Data.Text.Lazy.Encoding

main :: IO ()
main = do
  inputPath <- generateInputPath
  inputBytes <- ByteString.readFile inputPath
  case fileParse inputBytes of
    Fail unconsumedBytes _ _ -> do
      putStrLn $ "Failure parsing file " ++ inputPath ++ " as JSON"
      putStrLn "Here are the first 10 characters of the failure point:"
      putStrLn $ UTF8.toString $ ByteString.take 10 unconsumedBytes
    Done unconsumedBytes jsonObject -> do
      when (ByteString.length unconsumedBytes > 0) $ 
           putStrLn ("JSON file had leftover bit: " ++ UTF8.toString unconsumedBytes)
      case extractTweets jsonObject of
        Error errorString -> putStrLn $ "Tweets parse error: " ++ errorString
        Success tweets -> 
          let orderedTweets = sortBy (compare `on` created_at) tweets
              groupedTweets = groupBy ((==) `on` (date . created_at)) orderedTweets
              datesAndTweets = [(date $ created_at $ head tweets, tweets) | tweets <- groupedTweets]
              datesAndHTML = [(date, makePage date tweets) | (date@Date{..}, tweets) <- datesAndTweets, 
                              printForDate year month dayOfMonth]
          in do mapM (uncurry writePage) datesAndHTML
                putStrLn $ "All done!"

writePage Date{..} page = do
  outputDirectory <- generateOutputDirectory
  let outputPath = outputDirectory </> (show year) </> (show month) </> (show dayOfMonth) <> ".html"
  -- We canonicalize the name just for more readable terminal messages in the next line
  putStrLn $ "Writing output to: " <> outputPath
  writeFile outputPath page

writeFile path (encodeUtf8 -> content) = do
  createDirectoryIfMissing True $ takeDirectory path
  ByteString.writeFile path content