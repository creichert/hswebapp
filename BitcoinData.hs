{-# LANGUAGE OverloadedStrings #-}
module BitcoinData ( BitcoinData(BitcoinData)
                   , btcTickerURL
                   ) where

import Control.Applicative
import Control.Monad (mzero)

import Data.Aeson
import Data.ByteString.Lazy

import Happstack.Server (ServerPart)

-- | Example API call to a ticker.
btcTickerURL :: String
btcTickerURL = "https://api.bitcoinaverage.com/ticker"

-- | Type representing recent bitcoin exchange data.
data BitcoinData = BitcoinData { avg       :: Float
                               , ask       :: Float
                               , bid       :: Float
                               , last      :: Float
                               , timestamp :: String
                               , total_vol :: Float
                               } deriving (Show)

-- | FromJSON serializer.
--
-- Forced to do boilerplate serialization due to the
-- param "24_avg" starting with a digit.
instance FromJSON BitcoinData where
    parseJSON (Object v) = BitcoinData <$> v .: "24h_avg"
                                       <*> v .: "ask"
                                       <*> v .: "bid"
                                       <*> v .: "last"
                                       <*> v .: "timestamp"
                                       <*> v .: "total_vol"
    parseJSON _ = mzero

-- | ToJSON serializer.
instance ToJSON BitcoinData where
    toJSON (BitcoinData avg ask bid last stamp vol) =
        object [ "24_avg"    .= avg
               , "ask"       .= ask
               , "bid"       .= bid
               , "last"      .= last
               , "timestamp" .= stamp
               , "total_vol" .= vol
               ]
