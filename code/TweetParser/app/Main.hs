{-# LANGUAGE OverloadedStrings #-}
module Main where

import Constants
import Parse
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy as ByteString
import Data.Attoparsec.ByteString.Lazy

main :: IO ()
main = do
  file <- ByteString.readFile inputPath
  putStrLn $ show $ ByteString.length file
  let x = fileParse file
  case x of
    Done _ _ -> putStrLn "Success"
    Fail i x z -> putStrLn $ UTF8.toString $ ByteString.take 10 i