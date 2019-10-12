{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import           Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (putStrLn)
-- import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
import Data.Maybe


instance FromJSON Flight where
  parseJSON = withObject "flight" $ \o ->
    Flight <$> o.: "distance"

gimmeID :: Value -> Parser String
gimmeID = withObject "String" $ \o -> do
  id <- o .: "search_id"
  return id

gimmeData :: Value -> Parser Array
gimmeData = withObject "Array" $ \o -> do
  dat <- o .: "data"
  return dat



data Flight = Flight {
  distance :: Float}

data Stuff = Stuff {
  data :: !Array
} deriving Show


main :: IO ()
main = do
    response <- httpJSON "https://api.skypicker.com/flights?fly_from=BOS&fly_to=CCS"
    Prelude.putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    let val = getResponseBody response :: Value
    let mymyohmy = fromJust $ parseMaybe gimmeData val
    Prelude.putStrLn mymyohmy
    -- Data.ByteString.Lazy.putStrLn $ encode val
