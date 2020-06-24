module Game (startGame, startFishing) where    

import Text.Read
import Data.Maybe
import Angler
import Weather
import System.Exit

angeln = "Angeln" 


startGame = do
    putStrLn "Herzlich Willkommen zur Angelsimulation!"
    putStrLn $"Tippe '" ++ angeln ++ "' ein, wenn du bereit bist zu angeln."
    input <- getLine
    checkForValidInput input
    currentWeather <- generateRandomWeatherCondition    
    showBestFishingSpot currentWeather
    if currentWeather == Hagel 
        then exitSuccess  -- Programm an dieser Stelle beenden
    else putStrLn $" gehen wir " ++ setLocation currentWeather ++ " angeln ..."

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
    putStrLn "...Nanu, es hat etwas angebissen!... Es ist ein ..."


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


showBestFishingSpot :: WeatherConditions -> IO()
showBestFishingSpot weatherCondString
    | weatherCondString == Sonnig = putStrLn "Super! Es ist sonnig, wir gehen ans Meer zum fischen!"
    | weatherCondString == Wolkig = putStrLn "Das Wetter ist nicht so gut, lass uns lieber an den nächsten See gehen!"
    | weatherCondString == Neblig = putStrLn "Es ist neblig. Du könntest die Fische nicht so gut sehen! Gehen wir an den Rhein!"
    | weatherCondString == Regnerisch =  putStrLn "Es regnet! Wir fahren besser nach Norwegen und fischen an einem Fjord. Da gibt es bestimmt frischen Lachs!"
    | weatherCondString == Gewitter = putStrLn "Pfui! Es sind starke Gewitter unterwegs. Wir gehen lieber zum Baggersee"
    | weatherCondString == Schneefall = putStrLn "Es schneit! Eisfischen in Sibiren ist auch ganz cool!"
    | weatherCondString == Hagel = putStrLn "Willst du wirklich Fischen gehen bei Hagel? Lass es besser!"

setLocation :: WeatherConditions -> String
setLocation Sonnig = "ans Meer"  
setLocation Wolkig = "zum See"  
setLocation Neblig = "zum Fluss" 
setLocation Regnerisch = "zum Fjord in Norwegen" 
setLocation Gewitter = "zum Baggersee"
setLocation Schneefall = "nach Sibirien"
setLocation Hagel = ""