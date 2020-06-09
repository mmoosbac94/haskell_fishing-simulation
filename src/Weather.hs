module Weather where

data Weather = Weather {
    name :: String,
    typ :: String,
    wind :: Int,
    temperature :: Int
} deriving (Show, Eq)
