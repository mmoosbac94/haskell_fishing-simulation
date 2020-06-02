module Lib
    ( someFunc,
    randomFunc
    ) where

import System.Random        

someFunc :: IO ()
someFunc = putStrLn "someFunc"

randomFunc = do
    gen <- getStdGen
    print $"Zufaellige Zahl: " ++ show (head (randomRs (500,10000) gen) :: Int)
