module Game (startGame, startFishing) where    

import Control.Concurrent
import System.Random

import Text.Read
import Data.Maybe
import Angler
import Fish

angeln = "Angeln"
los = "Los gehts!"
ready = "R"

startGame = do
    putStrLn "Herzlich Willkommen zur Angelsimulation!"
    putStrLn $"Tippe '" ++ angeln ++ "' ein, wenn du bereit bist zu angeln."
    input <- getLine
    checkForValidAngelnInput input

createAngler = do
    putStrLn "Alles klar, wir wüssten natürlich gerne noch mit wem wir es heute zu tun haben!"
    putStrLn "Bitte gib zunächst deinen Namen ein: "
    anglerName <- getLine
    putStrLn $"Okay, du bist also " ++ anglerName ++ ". Es freut uns dich kennenzulernen!"
    putStrLn "Bitte gib jetzt dein Alter ein: "
    anglerAgeTest <- getLine
    anglerAge <- checkForValidAge anglerAgeTest
    let angler = Angler {name = anglerName, age = anglerAge}
    putStrLn $"Vielen Dank! Du heisst also " ++ name angler ++ " und bist " ++ show(age angler) ++ " Jahre alt."
    putStrLn $"Wenn du loslegen möchtest tippe '" ++ los ++ "' ein."
    input <- getLine
    checkForValidLosInput input

startFishing = do
    putStrLn "Alles klar, Angel wird ausgeworfen..."
    randomDelay <- generateRandomDelay
    print randomDelay
    threadDelay randomDelay
    generatedFish <- generateFish
    putStrLn $"...Nanu, es hat etwas angebissen! Es ist ein " ++ show(fishName generatedFish) ++ " mit einem Gewicht von " ++ 
        show(fishWeight generatedFish) ++ " g und einer Länge von " ++ show(fishLength generatedFish) ++ " cm."
    putStrLn $"Du hast noch nicht genug? Wirf die Angeln nochmal aus, indem du '" ++ ready ++ "' eingibst."    
    input <- getLine
    checkForValidReadyInput input
    startFishing


checkForValidAngelnInput :: String -> IO()
checkForValidAngelnInput input
    | input == angeln = createAngler
    | otherwise = do
        putStrLn $"Du musst '" ++ angeln ++ "' eingeben!"
        input <- getLine
        checkForValidAngelnInput input        

checkForValidLosInput :: String -> IO()
checkForValidLosInput input
    | input == los = startFishing
    | otherwise = do
        putStrLn $"Du musst '" ++ los ++ "' eingeben!"
        input <- getLine
        checkForValidLosInput input        

checkForValidReadyInput :: String -> IO()
checkForValidReadyInput input
    | input == ready = startFishing
    | otherwise = do
        putStrLn $"Du musst '" ++ ready ++ "' eingeben!"
        input <- getLine
        checkForValidReadyInput input    

checkForValidAge :: String -> IO Int
checkForValidAge anglerAge
    -- 'return' wandelt Int in IO Int um + 'read' castet String in Int (weil vorher mit Maybe bereits überprüft)
    | isJust (readMaybe anglerAge :: Maybe Int) = return (read anglerAge :: Int)
    | otherwise = do
        putStrLn "Butter bei die Fische, gib ein valides Alter ein..."
        input <- getLine
        checkForValidAge input

generateRandomDelay :: IO Int
generateRandomDelay = do
    gen <- newStdGen
    return (head (randomRs (1000000,10000000) gen) :: Int)        