{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Weather where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

import System.Random


data Weather = Weather {
    main :: String,     
    temperature :: Int
} deriving (Show, Eq, Generic)


data WeatherConditions = Sonnig | Wolkig | Neblig | Regnerisch | Gewitter | Schneefall | Hagel deriving (Show, Eq)
weather = [Sonnig,Wolkig,Neblig,Regnerisch,Gewitter,Schneefall,Hagel]

generateRandomTemp :: IO Int
generateRandomTemp = do
    gen <- newStdGen
    return (head (randomRs (-20,40) gen) :: Int)


generateRandomWeatherCondition :: IO WeatherConditions
generateRandomWeatherCondition = do 
    gen <- newStdGen
    let number = head (randomRs (0,length weather - 1) gen) :: Int
    return $weather!!number

generateWeather :: IO Weather
generateWeather = do
    wname <- generateRandomWeatherCondition
    let weatherCondString = show wname
    temperature <- generateRandomTemp    
    return Weather {main=weatherCondString, temperature=temperature}
    