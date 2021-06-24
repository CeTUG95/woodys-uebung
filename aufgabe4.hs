module Main where
import System.IO

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
getAdresse (Adresse vorname nachname strasse hausnummer plz stadt telefon) = vorname ++ ";" ++ nachname ++ ";" ++ strasse ++ ";" ++ hausnummer ++ ";" ++ plz ++ ";" ++ stadt ++ ";" ++ telefon ++ "\n"

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
            appendFile file (getAdresse adresseNeu)
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


    -- -- Dateiinhalt lesen und in variable Speichern
    -- addresses <- readFile file -- ohne zwischenschritt zwischen Lesen und schreiben, ist die Datei noch "locked"
    -- putStrLn ("Alte Datei:\n" ++ addresses)
    --
    -- -- neue Addresse deklarieren und in die Datei schreiben
    -- let addr = Address "Testo" "Testodor" "Testweg" "999" "54321" "Testdorf" "0198765432"
    -- writeFile file (addresses ++ getAddress addr)
    --
    -- -- erneut auslesen und printen
    -- newAddresses <- readFile file
    -- putStrLn ("Neue Datei:\n" ++ newAddresses)





-- Konzept:
-- in do Block:
--      - Abfrage was man tun möchte:
--          - 1 = Hinzufügen
--          - 2 = Löschen
--          - 3 = Editieren
--          - 4 = Alle anzeigen
--          - 5 = nach Vornamen suchen
--          - 6 = nach Nachnamen suchen
-- nach jeder dieser Operationen schreibzugriff auf Datei um neuen Stand abzuspeichern
-- ich weiß noch nicht wie genau das umgesetzt wird mit den unterschiedlichen Zahlen, die zu den Aktionen führen sollen
-- andere Idee vielleicht besser?
