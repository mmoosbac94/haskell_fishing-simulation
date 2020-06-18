module Fish (Fish(..), generateFish) where

import System.Random

-- Record Syntax
data Fish = Fish {
    fishName :: String,
    fishLength :: Int,
    fishWeight :: Int
} deriving (Show, Eq)

data SmallFishType = Goldfisch | Guppi | Sardine deriving (Show)
data BigFishType = Hai | Barsch | Zander deriving (Show)

smallFishes = [Goldfisch, Guppi, Sardine]
bigFishes = [Hai, Barsch, Zander]

generateRandomWeight :: IO Int
generateRandomWeight = do
    gen <- newStdGen
    return (head (randomRs (500,10000) gen) :: Int)

generateRandomLength :: IO Int
generateRandomLength = do
    gen <- newStdGen
    return (head (randomRs (3,100) gen) :: Int)

generateRandomFishName :: IO SmallFishType
generateRandomFishName = do
    gen <- newStdGen
    let number = (head (randomRs (0,2) gen) :: Int)
    return $smallFishes!!number

generateFish :: IO Fish
generateFish = do
    fishName <- generateRandomFishName
    let rightName = show fishName
    weight <- generateRandomWeight
    length <- generateRandomLength
    return Fish {fishName=rightName, fishLength=length, fishWeight=weight}