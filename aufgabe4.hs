module Main where
import System.IO
import Data.List
import qualified Data.Text as Text

-- deklaration des files
file = "addresses.txt"

-- Adresse mit Vorname, Name, Straße, Hausnummer, Postleitzahl, Stadt und Telefonnummer
data Adresse = Adresse
    {
        vorname :: String,
        nachname :: String,
        strasse :: String,
        hausnummer :: String,
        plz :: String,
        stadt :: String,
        telefon :: String
    } deriving (Ord, Eq, Show)

getAdresse :: Adresse -> String
getAdresse (Adresse vorname nachname strasse hausnummer plz stadt telefon) = vorname ++ ";" ++ nachname ++ ";" ++ strasse ++ ";" ++ hausnummer ++ ";" ++ plz ++ ";" ++ stadt ++ ";" ++ telefon

getValue :: String -> Int -> String
getValue [] index = ""
getValue string index = do
    let list = map Text.unpack $ Text.splitOn (Text.pack ";") (Text.pack string)
    list!!index

printAdresse :: String -> IO()
printAdresse [] = putStrLn ""
printAddresse addr = do
    let vorname = getValue addr 0
    let nachname = getValue addr 1
    let strasse = getValue addr 2
    let hausnummer = getValue addr 3
    let stadt = getValue addr 4
    let telefon = getValue addr 5
    putStrLn ("Vorname:\t" ++ vorname)
    putStrLn ("Nachname:\t" ++ nachname)
    putStrLn ("Straße:\t\t" ++ strasse)
    putStrLn ("Hausnummer:\t" ++ hausnummer)
    putStrLn ("Stadt:\t\t" ++ stadt)
    putStrLn ("Telefon:\t" ++ telefon ++ "\n")

printAdressListe :: [String] -> IO()
printAdressListe [] = return ()
printAdressListe (x:xs) = do
    printAddresse x
    printAdressListe xs


main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "\n\n===================================="
    putStrLn "Waehle einer der folgenden Aktionen:"
    putStrLn "1. Kontakt hinzufuegen"
    putStrLn "2. Kontakt loeschen"
    putStrLn "3. Kontakt editieren"
    putStrLn "4. Alle Kontakte anzeigen"
    putStrLn "5. Kontakt mit Vornamen suchen"
    putStrLn "6. Kontakt mit Nachnamen suchen"
    putStrLn "7. Beenden"
    putStrLn "------------------------------------"
    putStr "Eingabe: "
    input <- readLn
    putStrLn "====================================\n"
    case input of
        1 -> do
            -- Erfassen der Eingabedaten
            putStrLn "Kontakt hinzfuegen:"
            putStr "    Vorname: "
            vorname <- getLine
            putStr "    Nachname: "
            nachname <- getLine
            putStr "    Strasse: "
            strasse <- getLine
            putStr "    Hausnummer: "
            hausnummer <- getLine
            putStr "    Postleitzahl: "
            plz <- getLine
            putStr "    Stadt: "
            stadt <- getLine
            putStr "    Telefonnummer: "
            telefon <- getLine
            -- neue Adresse erstellen
            let adresseNeu = Adresse vorname nachname strasse hausnummer plz stadt telefon
            appendFile file ((getAdresse adresseNeu) ++ "\n")
            -- das newline sorgt dummerweise dafür dass es immer eine Zeile mehr im File gibt als es Addressen gibt.
            -- Das führt zu dem Fehler, dass beim lesen aller Kontakte bei 4. das Programm immer einen leeren Kontakt printed
            putStrLn "\nAdresse gespeichert.\n"
            main

        2 -> do
            putStrLn "Kontakt loeschen:"
            main

        3 -> do
            putStrLn "Kontakt editieren:"
            main

        4 -> do
            putStrLn "Alle Kontakte:"
            putStrLn "------------------------------------\n"
            adressenRaw <- readFile file
            let list = map Text.unpack $ Text.splitOn (Text.pack "\n") (Text.pack adressenRaw)
            printAdressListe list
            putStrLn "------------------------------------"

            main

        5 -> do
            putStrLn "Kontakt mit Vornamen suchen:"
            main

        6 -> do
            putStrLn "Kontakt mit Nachnamen suchen:"
            main

        7 -> do
            putStrLn "Auf Wiedersehen!"

        _ -> do
            putStrLn "Fehler: Ungueltige Eingabe! Bitte waehle eine Zahl von 1 bis 7."
            main
