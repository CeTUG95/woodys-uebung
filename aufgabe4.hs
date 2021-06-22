-- Adresse mit Vorname, Name, Straße, Hausnummer, Postleitzahl, Stadt und Telefonnummer
data Address = Address
    {
        firstname :: String,
        lastname :: String,
        street :: String,
        housenumber :: String,
        zipcode :: String,
        town :: String,
        telephone :: String
    } deriving (Ord, Eq, Show)

getAddress :: Address -> String
getAddress (Address firstName lastName street housenumber zipcode town telephone) = firstName ++ "," ++ lastName ++ "," ++ street ++ "," ++ housenumber ++ "," ++ zipcode ++ "," ++ town ++ "," ++ telephone ++ "\n"

main = do
    -- Datei festlegen
    let file = "addresses.txt"

    -- Dateiinhalt lesen und in variable Speichern
    addresses <- readFile file -- ohne zwischenschritt zwischen Lesen und schreiben, ist die Datei noch "locked"
    putStrLn ("Alte Datei:\n" ++ addresses)

    -- neue Addresse deklarieren und in die Datei schreiben
    let addr = Address "Testo" "Testodor" "Testweg" "999" "54321" "Testdorf" "0198765432"
    writeFile file (addresses ++ getAddress addr)

    -- erneut auslesen und printen
    newAddresses <- readFile file
    putStrLn ("Neue Datei:\n" ++ newAddresses)





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
