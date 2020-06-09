module Fish (Fish(..), generateFish) where

import System.Random    

-- Record Syntax
data Fish = Fish {
    fishName :: String,
    fishLength :: Int,
    fishWeight :: Int
} deriving (Show)


data SmallFishes = Goldfisch | Guppi | Sardine
data BigFishes = Hai | Barsch | Zander deriving (Enum)

generateRandomWeight :: IO()
generateRandomWeight = do
    gen <- getStdGen
    print $"Zufällige Zahl: " ++ show (head (randomRs (500,10000) gen) :: Int)

generateFish :: Fish
generateFish = Fish {fishName ="Goldfisch", fishLength=10, fishWeight=200}

    