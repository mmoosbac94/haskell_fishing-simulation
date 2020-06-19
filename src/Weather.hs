{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Weather where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics


data Weather = Weather {
    main :: String,
    description :: String,
    wind :: Int,
    temperature :: Float
} deriving (Show, Eq, Generic)

-- instance FromJSON Weather

instance FromJSON Weather where
 parseJSON (Object v) =
    Weather <$> v .: "main"
           <*> v .: "description"
           <*> v .: "wind"
           <*> v .: "temperature"

instance ToJSON Weather where
 toJSON (Weather main description wind temperature) =
    object [ "main"         .= main
           , "description"  .= description
           , "wind"         .= wind
           , "temperature"  .= temperature
             ]


apiKey = "441e81fb7b249e1a876e04a163f30716"

stadt = "cologne"


jsonURL :: String
jsonURL = "http://api.openweathermap.org/data/2.5/weather?q=" ++ stadt ++ "&appid=" ++ apiKey

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

convertData :: IO()
convertData = do
    -- get JSON data and encode it
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Weather])
    -- If d is Left, the JSON was malformed.
    -- In that case, we report the error.
    -- Otherwise, we perform the operation of
    -- our choice. In this case, just print it. 
    case d of
        Left err -> putStrLn err
        Right w -> print w 

{-
jsonConverter :: IO()
jsonConverter = do
    input <- getJSON
    let mm = decode input :: Maybe Weather
    case mm of
        Nothing -> print "Error in parsing JSON"
        Just  -> decode input
-}
-- showingWeather w = (show.name) w ++ " , " ++ (show.temperature) w



