module Game (startGame, startFishing) where    

import Text.Read
import Data.Maybe
import Angler
import Fish

angeln = "Angeln" 

startGame = do
    putStrLn "Herzlich Willkommen zur Angelsimulation!"
    putStrLn $"Tippe '" ++ angeln ++ "' ein, wenn du bereit bist zu angeln."
    input <- getLine
    checkForValidInput input

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

startFishing = do
    putStrLn "Lass uns die Angel auswerfen..."
    putStrLn "(Delay)"
    generatedFish <- generateFish
    putStrLn $"...Nanu, es hat etwas angebissen!... Es ist ein ... " ++ show(generatedFish)
    input <- getLine
    startFishing


checkForValidInput :: String -> IO()
checkForValidInput input
    | input == angeln = createAngler
    | otherwise = do
        putStrLn $"Du musst '" ++ angeln ++ "' eingeben, um fortfahren zu können..."
        input <- getLine
        checkForValidInput input

checkForValidAge :: String -> IO Int
checkForValidAge anglerAge
    -- 'return' wandelt Int in IO Int um + 'read' castet String in Int (weil vorher mit Maybe bereits überprüft)
    | isJust (readMaybe anglerAge :: Maybe Int) = return (read anglerAge :: Int)
    | otherwise = do
        putStrLn "Butter bei die Fische, gib ein valides Alter ein..."
        input <- getLine
        checkForValidAge input