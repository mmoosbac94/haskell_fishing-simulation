module Game where    

import Angler

startGame = do
    putStrLn "Herzlich Willkommen zur Angelsimulation!"
    putStrLn "Tippe 'F' ein, wenn du bereit bist fortzufahren."
    input <- getLine
    checkForInput input

createAngler = do
    putStrLn "Alles klar, wir wüssten natürlich gerne noch mit wem wir es heute zu tun haben!"
    putStrLn "Bitte gib zunächst deinen Namen ein: "
    anglerName <- getLine
    putStrLn $"Okay, du bist also " ++ anglerName ++ ". Es freut uns dich kennenzulernen!"
    putStrLn "Bitte gib jetzt dein Alter ein: "
    anglerAge <- getLine
    let angler = Angler {name = anglerName, age = anglerAge}
    putStrLn $"Vielen Dank! Du heisst also " ++ name angler ++ " und bist " ++ age angler ++ " alt."

checkForInput :: String -> IO()
checkForInput input
    | input == "F" = createAngler
    | otherwise = do
        putStrLn "Du musst 'F' eingeben, um fortfahren zu können..."
        input <- getLine
        checkForInput input    


