module Main where

import Game
import OpenweatherAPI

main = do
    startGame
    startFishing
    makeRequest "cologne"