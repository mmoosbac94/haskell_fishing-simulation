module Game (startGame, startFishing) where    

import Control.Concurrent
import System.Random
import Data.Typeable

import Text.Read
import Data.Maybe
import Angler
import Weather
import System.Exit
import Fish

angeln = "Angeln"
ready = "R"
los = "Los!"


startGame = do
    putStrLn "Herzlich Willkommen zur Angelsimulation!"
    putStrLn $"Tippe '" ++ angeln ++ "' ein, wenn du bereit bist zu angeln."
    input <- getLine

    checkForValidAngelnInput input

    weather <- generateWeather
    putStrLn $"Das Wetter: " ++ show(main weather) ++ " und eine Temperatur von " ++ show(temperature weather) ++ " Grad Celsius."        
    showBestFishingSpot $main weather
    let location = setLocation (main weather)
    if main weather == Hagel
        then exitSuccess  -- Programm an dieser Stelle beenden    
    else print $" gehen wir " ++ location ++ " angeln ..."


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

startFishing :: [Fish] -> IO()
startFishing fishBag = do
    putStrLn "Alles klar, Angel wird ausgeworfen..."
    randomDelay <- generateRandomDelay
    print randomDelay
    threadDelay randomDelay

    generatedFish <- generateFish
    let weight = fishWeight generatedFish
    let isBig = checkifBigFish weight
    printSuccessBasedOnWeight isBig generatedFish
    
    putStrLn $"Möchtest du die/den " ++ show(fishName generatedFish) ++ " in die Tasche stecken? (Ja/Nein)"
    input <- getLine
    newFishBag <- checkPutInFishBag input generatedFish fishBag
    if fishBag == newFishBag then putStrLn "Fisch wurde freigelassen" else putStrLn "Fisch wurde in Tasche gepackt"
    putStrLn $"Deine Tasche: " ++ show newFishBag
    
    putStrLn $"Du hast noch nicht genug? Wirf die Angeln nochmal aus, indem du '" ++ ready ++ "' eingibst."    
    input <- getLine
    checkForValidReadyInput input newFishBag


printSuccessBasedOnWeight :: Bool -> Fish -> IO()
printSuccessBasedOnWeight isBig generatedFish
    | isBig = putStrLn $"...Nanu, es hat etwas GROßES angebissen! Es ist ein/e " ++ show(fishName generatedFish) ++ " mit einem Gewicht von " ++ show(fishWeight generatedFish) ++ " g und einer Länge von " ++ show(fishLength generatedFish) ++ " cm."
    | otherwise = putStrLn $"...Nanu, es hat nur etwas Kleines angebissen! Es ist ein/e " ++ show(fishName generatedFish) ++ " mit einem Gewicht von " ++ show(fishWeight generatedFish) ++ " g und einer Länge von " ++ show(fishLength generatedFish) ++ " cm."    


checkPutInFishBag :: String -> Fish -> [Fish] -> IO [Fish]
checkPutInFishBag input generatedFish fishBag
    | input == "Ja" = return $generatedFish : fishBag
    | input == "Nein" = return fishBag
    | otherwise = do
        putStrLn "Gib Ja oder Nein ein!"
        input <- getLine
        checkPutInFishBag input generatedFish fishBag


checkForValidAngelnInput :: String -> IO()
checkForValidAngelnInput "Angeln" = createAngler
checkForValidAngelnInput x = do
    putStrLn $"Du musst '" ++ angeln ++ "' eingeben!"
    input <- getLine
    checkForValidAngelnInput input    


checkForValidLosInput :: String -> IO()
checkForValidLosInput "Los!" = startFishing []
checkForValidLosInput x = do
    putStrLn $"Du musst '" ++ los ++ "' eingeben!"
    input <- getLine
    checkForValidLosInput input  
      

checkForValidReadyInput :: String -> [Fish] -> IO()
checkForValidReadyInput input fishBag
    | input == ready = startFishing fishBag
    | otherwise = do
        putStrLn $"Du musst '" ++ ready ++ "' eingeben!"
        input <- getLine
        checkForValidReadyInput input fishBag    

checkForValidAge :: String -> IO Int
checkForValidAge anglerAge
    -- isJust prüft, ob der übergebene Wert der Form Just_ entspricht, wobei readMaybe ein Just_ oder ein Nothing zurückgibt, je nachdem ob der String als Int gelesen werden kann
    -- 'return' wandelt Int in IO Int um + 'read' castet String in Int (weil vorher mit Maybe bereits überprüft)
    -- -> HighOrder-Function weil einer Funktion als Parameter eine Funktion übergeben wird
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

generateRandomDelay :: IO Int
generateRandomDelay = do
    gen <- newStdGen
    return (head (randomRs (1000000,10000000) gen) :: Int)
