{-# LANGUAGE OverloadedStrings #-}

module OpenweatherAPI where

import Data.Aeson
import Network.Browser
import Network.HTTP
import Network.HTTP.Base

apiKey = "441e81fb7b249e1a876e04a163f30716"

makeRequest :: String -> Response
makeRequest city = do
    rsp <- Network.HTTP.simpleHTTP (getRequest "api.openweathermap.org/data/2.5/weather?q=" ++ city ++ "&appid=" ++ apiKey)
             -- fetch document and return it (as a 'String'.)
    fmap (take 100) (getResponseBody rsp)
    do
     (_, rsp)
        <- Network.Browser.browse $ do
              setAllowRedirects True -- handle HTTP redirects
              request $ getRequest "api.openweathermap.org/data/2.5/weather?q=" ++ city ++ "&appid=" ++ apiKey
     return (take 100 (rspBody rsp))
