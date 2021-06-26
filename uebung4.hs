module Main where
import System.IO
import Control.Monad
import Data.Functor
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

deleteAdresse indexPerson text = firstIndex ++ lastIndex
    where (firstIndex, _:lastIndex) = splitAt indexPerson text

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

-- String anhand eines characters auseinander schneiden
cutString :: Char-> String-> String
cutString _ [] = []
cutString c (x:xs) | c==x = []
                    | otherwise = x:cutString c xs

-- neue Zeile nach Zeile aus Liste von Zeilen erstellen
createNewLine :: [String] -> String
createNewLine [] = ""
createNewLine [x] = x
createNewLine ( x:xs ) = x ++  "\n" ++ createNewLine xs

kontaktHinzufuegen :: IO()
kontaktHinzufuegen = do
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

kontaktLoeschen :: IO()
kontaktLoeschen = do
					putStrLn "Kontakt loeschen:"
					putStrLn "------------------------------------\n"
					putStr "Vornamen eingeben: "
					vornameInput <- getLine
					let lVornameInput = [vornameInput]
					adressenRaw <- readFile file
					let lAdressZeilen = lines adressenRaw -- Liste der Adresszeilen
					let lVornamen = map (cutString ';') (lines adressenRaw) -- Liste der Vornamen
					let lIndices = map (`elemIndices` lVornamen) lVornameInput
					let lIndex = head lIndices
					let lDelete = deleteAdresse (head $ head lIndices) lAdressZeilen
					let delete = filter (not . null) lDelete
					when (length (delete) > 0) $
						writeFile file (createNewLine delete)
					putStrLn ("Der Kontakt " ++ vornameInput ++ " wurde gelöscht!")
					putStrLn "------------------------------------\n"
					main
					
kontaktEditieren :: IO()
kontaktEditieren = do
					putStrLn "Kontakt editieren:"
					putStrLn "Bitte geben Sie den Vornamen ein, den Sie bearbeiten wollen!"
					-- get Person by Vorname 
					vornameInput <- getLine
					let lVornameInput = [vornameInput]
					adressenRaw <- readFile file
					let lAdressZeilen = lines adressenRaw -- Liste der Adresszeilen
					let lVornamen = map (cutString ';') (lines adressenRaw) -- Liste der Vornamen
					let lIndices = map (`elemIndices` lVornamen) lVornameInput
					let lIndex = head lIndices
					let editedPerson = map (lAdressZeilen !!) lIndex
					
					let personListString = map Text.unpack $ Text.splitOn (Text.pack ";") (Text.pack $ head editedPerson)
					let oldVorname = head personListString
					let	oldNachname = personListString!!1
					let	oldStrasse = personListString!!2
					let	oldHausnummer = personListString!!3
					let oldPlz = personListString!!4
					let oldStadt = personListString!!5
					let	oldTelefon = personListString!!6
					putStrLn ("Bitte geben Sie den neuen Vornamen ein | Der Alte ist: ->" ++ oldVorname ++"<- Wenn Sie den Vornamen nicht ändern wollen, geben Sie bitte 'ENTER' ein")
					newVorname <- getLine
					putStrLn ("Bitte geben Sie den neuen Nachnamen ein | Der Alte ist: ->" ++ oldNachname ++ "<- Wenn Sie den Nachnamen nicht ändern wollen, geben Sie bitte 'ENTER' ein")
					newNachname <- getLine
					putStrLn ("Bitte geben Sie die neue Straße ein | Die Alte ist: ->" ++ oldStrasse ++"<- Wenn Sie die Straße nicht ändern wollen, geben Sie bitte 'ENTER' ein")
					newStrasse <- getLine
					putStrLn ("Bitte geben Sie die neue Hausnummer ein | Die Alte ist: ->" ++ oldHausnummer ++"<- Wenn Sie die Hausnummer nicht ändern wollen, geben Sie bitte 'ENTER' ein")
					newHausnummer <- getLine
					putStrLn ("Bitte geben Sie die Postleitzahl ein | Die Alte ist: ->"  ++ oldPlz ++ "<- Wenn Sie die Postleitzahl nicht ändern wollen, geben Sie bitte 'ENTER' ein")
					newPlz <- getLine
					putStrLn ("Bitte geben Sie die Stadt  ein | Die Alte ist: ->"  ++ oldStadt ++ "<- Wenn Sie die Stadt nicht ändern wollen, geben Sie bitte 'ENTER' ein")
					newStadt <- getLine
					putStrLn ("Bitte geben Sie Ihre Telefonnummer ein | Die Alte ist: ->" ++ oldTelefon ++"<- Wenn Sie die Telefonnummer nicht ändern wollen, geben Sie bitte 'ENTER' ein")
					newTelefon <- getLine
					let vornameToAdd = if null newVorname
										then oldVorname
										else newVorname
					let nachnameToAdd = if null newNachname
										then oldNachname
										else newNachname
					let strasseToAdd = if null newStrasse
										then oldStrasse
										else newStrasse
					let hausnummerToAdd = if null newHausnummer
											then oldHausnummer
											else newHausnummer
					let plzToAdd = if null newPlz
										then oldPlz
										else newPlz
					let stadtToAdd = if null newStadt
										then oldStadt
										else newStadt
					let telefonToAdd = if null newTelefon
										then oldTelefon
										else newTelefon
					
					let adresseNeu = Adresse vornameToAdd nachnameToAdd strasseToAdd hausnummerToAdd plzToAdd stadtToAdd telefonToAdd
										
					-- delete and add
					-- crasht wenn es nicht die letzte Adresse ist ...
					adressenRaw <- readFile file
					let lAdressZeilen = lines adressenRaw -- Liste der Adresszeilen
					let lVornamen = map (cutString ';') (lines adressenRaw) -- Liste der Vornamen
					let lIndices = map (`elemIndices` lVornamen) lVornameInput
					let lIndex = head lIndices
					let lDelete = deleteAdresse (head $ head lIndices) lAdressZeilen ++ [(getAdresse adresseNeu) ++ "\n"]
					let delete = filter (not . null) lDelete
					when (length (delete) > 0) $
						writeFile file (createNewLine delete)
					
					putStrLn "Kontakt editieren erfolgreich!"
					main
					
alleKontakteAnzeigen :: IO()
alleKontakteAnzeigen = do
						putStrLn "Alle Kontakte:"
						putStrLn "------------------------------------\n"
						adressenRaw <- readFile file
						let list = map Text.unpack $ Text.splitOn (Text.pack "\n") (Text.pack adressenRaw)
						printAdressListe list
						putStrLn "------------------------------------"
						main

kontakteMitVornamenSuchen :: IO()
kontakteMitVornamenSuchen = do
								putStrLn "Kontakt mit Vornamen suchen:"
								putStrLn "------------------------------------\n"
								putStr "Vornamen eingeben: "
								vornameInput <- getLine
								let lVornameInput = [vornameInput]
								adressenRaw <- readFile file
								let lAdressZeilen = lines adressenRaw -- Liste der Adresszeilen
								let lVornamen = map (cutString ';') (lines adressenRaw) -- Liste der Vornamen
								let lIndices = map (`elemIndices` lVornamen) lVornameInput
								let lIndex = head lIndices
								putStrLn "Suchergebnisse:"
								printAdressListe $ map (lAdressZeilen !!) lIndex
								putStrLn "------------------------------------\n"
								main

kontakteMitNachnamenSuchen :: IO()
kontakteMitNachnamenSuchen = do
								putStrLn "Kontakt mit Nachnamen suchen:"
								putStrLn "------------------------------------\n"
								putStr "Nachname eingeben: "
								nachnameInput <- getLine
								let lNachnameInput = [nachnameInput] -- umwandeln in Liste für einfacheres Handling danach
								adressenRaw <- readFile file
								let lAdressZeilen = lines adressenRaw -- Liste aller Adresszeilen
								let lAdressString = map (map Text.unpack . Text.splitOn (Text.pack ";") . Text.pack) lAdressZeilen
								let lAdressNamen = map tail lAdressString
								let lNachnamen = map head lAdressNamen
								putStrLn ("lAdressString: " ++ show lAdressString)
								putStrLn ("lAdressNamen: " ++ show lAdressNamen)
								putStrLn ("lNachnamen: " ++ show lNachnamen)
								let lIndices = map (`elemIndices` lNachnamen) lNachnameInput
								let lHeadIndices = head lIndices
								putStrLn "Suchergebnisse:"
								printAdressListe $ map (lAdressZeilen !!) lHeadIndices
								putStrLn "------------------------------------\n"
								main

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
        1 -> kontaktHinzufuegen
        2 -> kontaktLoeschen
        3 -> kontaktEditieren
        4 -> alleKontakteAnzeigen
        5 -> kontakteMitVornamenSuchen
        6 -> kontakteMitNachnamenSuchen
        7 -> do
            putStrLn "Auf Wiedersehen!"
        _ -> do
            putStrLn "Fehler: Ungueltige Eingabe! Bitte waehle eine Zahl von 1 bis 7."
            main