module Fish (Fish(..), generateFish, checkifBigFish) where

import System.Random
import Weather

-- Record Syntax
data Fish = Fish {
    fishName :: String,
    fishLength :: Int,
    fishWeight :: Int
} deriving (Show, Eq)

data FishType = Barbe | Brasse | Barsch | Bachforelle | Hasel | Hecht | Karpfen | Regenbogenforelle | Rotauge | Rotfeder | 
    Dorsch | Makrele | Hering | Scholle | Lachs | Thunfisch | Seezunge | Steinbutt | Tintenfisch | Kabeljau deriving (Show)

lakeFish = [Barbe, Brasse, Barsch, Bachforelle, Hasel, Hecht, Karpfen, Regenbogenforelle, Rotauge, Rotfeder]
seaFish = [Dorsch, Makrele, Hering, Scholle, Lachs, Thunfisch, Seezunge, Steinbutt, Tintenfisch, Kabeljau]


generateRandomWeight :: IO Int
generateRandomWeight = do
    gen <- newStdGen
    return (head (randomRs (500,10000) gen) :: Int)

generateRandomLength :: IO Int
generateRandomLength = do
    gen <- newStdGen
    return (head (randomRs (3,100) gen) :: Int)

generateRandomFishName :: WeatherConditions -> IO FishType
generateRandomFishName checkedWeather = do
    gen <- newStdGen
    if checkedWeather == Sonnig || checkedWeather == Wolkig then do
        -- Weiteres Beispiel für HighOrder-Function (Der Funktion Head wird die Funktion randomRs übergeben)
        let number = (head (randomRs (0,length seaFish - 1) gen) :: Int)
        return $seaFish!!number
    else do
        let number = (head (randomRs (0,length lakeFish - 1) gen) :: Int)
        return $lakeFish!!number


generateFish :: WeatherConditions -> IO Fish 
generateFish checkedWeather = do
        fishName <- generateRandomFishName checkedWeather
        let fishNameString = show fishName
        weight <- generateRandomWeight
        length <- generateRandomLength
        return Fish {fishName=fishNameString, fishLength=length, fishWeight=weight}  

checkifBigFish :: Int -> Bool
checkifBigFish weight
    | weight >= 5000 = True
    | otherwise = False