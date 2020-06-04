module Fish where

import System.Random    

-- Record Syntax
data Fish = Fish {
    name :: String,
    length :: Int,
    weight :: Int
} deriving (Show)


generateRandomWeight = do
    gen <- getStdGen
    print $"Zufällige Zahl: " ++ show (head (randomRs (500,10000) gen) :: Int)