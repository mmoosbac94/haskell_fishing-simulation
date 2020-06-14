module Fish (Fish(..), generateFish, generateRandomWeight) where

import System.Random

-- Record Syntax
data Fish = Fish {
    fishName :: String,
    fishLength :: Int,
    fishWeight :: Int
} deriving (Show)


data SmallFishes = Goldfisch | Guppi | Sardine
data BigFishes = Hai | Barsch | Zander deriving (Enum)

generateRandomWeight :: IO Int
generateRandomWeight = do
    gen <- newStdGen
    return (head (randomRs (500,10000) gen) :: Int)

generateRandomLength :: IO Int
generateRandomLength = do
    gen <- newStdGen
    return (head (randomRs (3,100) gen) :: Int)

--generateRandomFishName :: IO String
--generateRandomFishName = do
        

generateFish :: IO Fish
generateFish = do
    weight <- generateRandomWeight
    length <- generateRandomLength
    return Fish {fishName ="Goldfisch", fishLength=length, fishWeight=weight}