module Fish (Fish(..), generateFish, checkifBigFish) where

import System.Random

-- Record Syntax
data Fish = Fish {
    fishName :: String,
    fishLength :: Int,
    fishWeight :: Int
} deriving (Show, Eq)

data FishType = Goldfisch | Karpfen | Lachs | Hai | Barsch | Zander deriving (Show)

fishes = [Goldfisch, Karpfen, Lachs, Hai, Barsch, Zander]

generateRandomWeight :: IO Int
generateRandomWeight = do
    gen <- newStdGen
    return (head (randomRs (500,10000) gen) :: Int)

generateRandomLength :: IO Int
generateRandomLength = do
    gen <- newStdGen
    return (head (randomRs (3,100) gen) :: Int)

generateRandomFishName :: IO FishType
generateRandomFishName = do
    gen <- newStdGen
    let number = (head (randomRs (0,length fishes - 1) gen) :: Int)
    return $fishes!!number

generateFish :: IO Fish
generateFish = do
    fishName <- generateRandomFishName
    let fishNameString = show fishName
    weight <- generateRandomWeight
    length <- generateRandomLength
    return Fish {fishName=fishNameString, fishLength=length, fishWeight=weight}

checkifBigFish :: Int -> Bool
checkifBigFish weight
    | weight >= 5000 = True
    | otherwise = False