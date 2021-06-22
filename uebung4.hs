-- Adressverwaltung
-- ghc zum kompilieren von der Datei

data Adressverwaltung = [Person]

data Person = {	Vorname::string,
				Nachname::string, 
				Straße::string, 
				Hausnummer::string, 
				Postleitzahl::string, 
				Stadt::string, 
				Telefonnummer::string }
main = ask start

ask :: Adressverwaltung -> IO ()
ask = putStrLn "You have ... Contacts. What do you want to do?"
	  >> putStrLn "Possible operations are: add, delete, change, searchByVorname, searchByNachname, exit"
	  >> getLine
	  >>= processOperation

processOperation :: Adressverwaltung -> String -> IO()
processOperation adressen input | null input = ask() adressen
								| input == "exit" = exit adressen
								| input == "add" = ask() interactiveAdd Adressverwaltung

interactiveAdd :: Adressverwaltung -> Adressverwaltung
interactiveAdd :: putStrLn "Bitte geb einen Vornamen ein:"
				>> getLine
				>> putStrLn "Bitte geb einen Nachname ein:"
				>> getLine
				>> putStrLn "Bitte geb eine Straße ein:"
				>> getLine
				>> putStrLn "Bitte geb eine Hausnummer ein:"
				>> getLine
				>> putStrLn "Bitte geb eine Postleitzahl ein:"
				>> getLine
				>> putStrLn "Bitte geb eine Telefonnummer ein:"
				>> getLine
				>>= add Adressverwaltung
-- getChar und IO wichtig			
-- http://zvon.org/other/haskell/Outputprelude/readFile_f.html
-- lesen aus einer Textdatei mit readFile
-- start:: Adressverwaltung
-- start = toAdressverwaltung readFile "/adressen.txt"

-- toAdressverwaltung

-- beim beenden Kontakte in eine Textdatei schreiben mit writeFile
-- http://zvon.org/other/haskell/Outputprelude/writeFile_f.html
-- exit

-- Hinzufügen eines Kontaktes
-- add:: String -> String -> String -> String -> String -> String -> Adressverwaltung -> Adressverwaltung

-- Löschen eines Kontaktes
-- delete

-- Editieren eines Kontaktes
-- change

-- Suchen von Kontakten nach Vornamen
-- searchByVorname

-- Suchen nach Kontakten nach Nachnamen 
-- searchByNachname