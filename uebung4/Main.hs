module Main where
--Filehandler closing muss angepasst werden bei einigen menüpunkten
import System.IO
import Control.Monad
import Data.Functor
import Data.List
import qualified Data.Text as Text

dataPath = "data.txt"

createNewLine :: [String] -> String
createNewLine [] = ""
createNewLine [x] = x
createNewLine ( x:xs ) = x ++  "\n" ++ createNewLine xs

showPerson :: String -> IO ()
showPerson [] = putStrLn ""
showPerson x = do
    let vorname = getValue x 0
    let nachname = getValue x 1
    let strass = getValue x 2
    let stadt = getValue x 3
    let handy = getValue x 4
    putStrLn ("Vorname:\t" ++ vorname)
    putStrLn ("Nachname:\t" ++ nachname)
    putStrLn ("Straße:\t\t" ++ strass)
    putStrLn ("Stadt:\t\t" ++ stadt)
    putStrLn ("Handynummer:\t" ++ handy ++ "\n")

printElements :: [String] -> IO()
printElements [] = return ()
printElements (x:xs) = do showPerson x
                          printElements xs

getValue :: String -> Int -> String
getValue string index = do
    let list = map Text.unpack $ Text.splitOn (Text.pack ";") (Text.pack string)
    list!!index

addNewPerson :: String -> FilePath -> IO String
addNewPerson file newPerson  = do
    x <- readFile file
    length x `seq` writeFile file newPerson
    appendFile file x
    return x

cutString :: Char-> String-> String
cutString _ [] = []
cutString c (x:xs) | c==x = []
                    | otherwise = x:cutString c xs

deletPerson indexPerson text = firstIndex ++ lastIndex
    where (firstIndex, _:lastIndex) = splitAt indexPerson text

main:: IO ()
main = do
    putStrLn "Adressverwaltung"
    putStrLn "1. Hinzufügen eines Kontakts"
    putStrLn "2. Löschen eines Kontakts"
    putStrLn "3. Editieren eines Kontakts"
    putStrLn "4. Anzeigen aller Kontakte"
    putStrLn "5. Suchen von Kontakten nach Vornamen"
    putStrLn "6. Suchen nach Kontakten nach Nachnamen"
    putStrLn "7. Beenden"
    response <- readLn
    case response of
        1 -> do
            putStrLn "Bitte geben Sie den Vornamen ein: "
            vorname <- getLine
            putStrLn "Bitte geben Sie den Nachnamen ein: "
            nachname <- getLine
            putStrLn "Bitte geben Sie die Adresse mit Hausnummer ein: "
            adresse <- getLine
            putStrLn "Bitte geben Sie die Postleitzahl und den Ort ein: "
            postCode <- getLine
            putStrLn "Bitte geben Sie die Telefonnummer ein: "
            telNummer <- getLine
            let newPerson = vorname ++ ";" ++ nachname ++ ";" ++ adresse ++ ";" ++ postCode ++ ";" ++ telNummer  ++  "\n"
            void $ addNewPerson dataPath newPerson
            putStrLn ("Der Kontakt " ++ vorname ++ " " ++ nachname ++ " wurde erfolgreich hinzugefügt!")
            putStrLn "Bitte geben Sie 'ENTER' ein um fortzufahren!"
            enter <- getLine
            main
        2 -> do
            putStrLn "Bitte geben Sie den Vornamen ein, den Sie löschen wollen!"
            userName <- getLine
            let userNameList = [userName]
            let list = []
            let listLines = []
            let listIndices = []
            handle <- openFile dataPath ReadWriteMode
            contents <- hGetContents handle
            let listLines =  lines  contents
            let listOnlyFirstName = map (cutString ';') (lines contents)
            let listIndices = map (`elemIndices` listOnlyFirstName) userNameList
            let listIndicesWithoutList =  head listIndices
            let deletedList = deletPerson (head $ head listIndices) listLines
            let finalDelete =  filter (not . null) deletedList
            when (length (finalDelete) > 0) $
                    writeFile dataPath (createNewLine finalDelete)
            putStrLn ("Der Kontakt " ++ userName ++ " wurde gelöscht!")
            putStrLn "Bitte geben Sie 'ENTER' ein um fortzufahren!"
            enter <- getLine
            main
        3 -> do
            putStrLn "Bitte geben Sie den Vornamen ein, den Sie bearbeiten wollen!"
            userName <- getLine
            let userNameList = [userName]
            let list = []
            let listLines = []
            let listIndices = []
            handle <- openFile dataPath ReadWriteMode
            contents <- hGetContents handle
            let listLines =  lines  contents
            let listOnlyFirstName = map (cutString ';') (lines contents)
            let listIndices = map (`elemIndices` listOnlyFirstName) userNameList
            let listIndicesWithoutList =  head listIndices
            let editedPerson = map (listLines !!) listIndicesWithoutList
            let personListString = map Text.unpack $ Text.splitOn (Text.pack ";") (Text.pack $ head editedPerson)
            let oldVorname = head personListString
                oldNachname = personListString!!1
                oldAdresse1 = personListString!!2
                oldAdresse2 = personListString!!3
                oldPhone = personListString!!4
            putStrLn ("Bitte geben Sie den neuen Vornamen ein | Der Alte ist: ->" ++ oldVorname ++"<- Wenn Sie den Vornamen nicht ändern wollen, geben Sie bitte 'ENTER' ein")
            newVorname <- getLine
            putStrLn ("Bitte geben Sie den neuen Nachnamen ein | Der Alte ist: ->" ++ oldNachname ++ "<- Wenn Sie den Nachnamen nicht ändern wollen, geben Sie bitte 'ENTER' ein")
            newNachname <- getLine
            putStrLn ("Bitte geben Sie die neue Straße und Hausnummer ein | Die Alte ist: ->" ++ oldAdresse1 ++"<- Wenn Sie die Straße und Hausnummer nicht ändern wollen, geben Sie bitte 'ENTER' ein")
            newAdresse <- getLine
            putStrLn ("Bitte geben Sie die Postleitzahl und Stadt z.B. 12345 Musterstadt ein | Die Alte ist: ->"  ++ oldAdresse2 ++ "<- Wenn Sie die Postleitzahl und Stadt nicht ändern wollen, geben Sie bitte 'ENTER' ein")
            newPostCode <- getLine
            putStrLn ("Bitte geben Sie Ihre Telefonnummer ein | Die Alte ist: ->" ++ oldPhone ++"<- Wenn Sie die Telefonnummer nicht ändern wollen, geben Sie bitte 'ENTER' ein")
            newPhoneNummer <- getLine
            let vorNameToAdd = if null newVorname
                                then oldVorname
                                else newVorname
            let nachNameToAdd = if null newNachname
                                then oldNachname
                                else newNachname
            let adresseToAdd = if null newAdresse
                                then oldAdresse1
                                else newAdresse
            let postCodeToAdd = if null newPostCode
                                then oldAdresse2
                                else newPostCode
            let phoneToAdd = if null newPhoneNummer
                                then oldPhone
                                else newPhoneNummer
            let deletedList = deletPerson (head (head listIndices)) listLines
            let finalDelete = filter (not . null) deletedList
            when (length (finalDelete) > 0) $
                    writeFile dataPath (createNewLine finalDelete)
            let editedPerson = vorNameToAdd ++ ";" ++ nachNameToAdd ++ ";" ++ adresseToAdd ++ ";" ++ postCodeToAdd ++ ";" ++ phoneToAdd  ++  "\n"
            hClose handle
            void $ addNewPerson dataPath editedPerson
            putStrLn "Kontakt editieren erfolgreich!"
            putStrLn "Bitte geben Sie 'ENTER' ein um fortzufahren!"
            enter <- getLine
            main
        4 -> do
            handle <- openFile dataPath ReadMode
            contents <- hGetContents handle
            let list = map Text.unpack $ Text.splitOn (Text.pack "\n") (Text.pack contents)
            printElements list
            hClose handle
            putStrLn "Bitte geben Sie 'ENTER' ein um fortzufahren!"
            enter <- getLine
            main
        5 -> do
            putStrLn "Bitte geben Sie den gesuchten Vornamen ein!"
            searchVorname <- getLine
            let listLines = []
            let listIndices = []
            handle <- openFile dataPath ReadMode
            contents <- hGetContents handle
            let listLines =  lines  contents
            let searchVornameList = [searchVorname]
            let listOnlyFirstName = map (cutString ';') (lines contents)
            let listIndices = map (`elemIndices` listOnlyFirstName) searchVornameList
            let listIndicesWithoutList =  head listIndices
            putStrLn ("Der Index der Linie in File aller gesuchten Elemente: " ++ show  listIndicesWithoutList)
            let lengthResult = length listIndicesWithoutList
            putStrLn (show  lengthResult ++ " Suchergebnis ")
            printElements $ map (listLines !!) listIndicesWithoutList
            hClose handle
            putStrLn "Bitte geben Sie 'ENTER' ein um fortzufahren!"
            enter <- getLine
            main
        6 -> do
            putStrLn "Bitte geben Sie den gesuchten Nachnamen ein!"
            lastName <- getLine
            let lastNameList = [lastName]
            let listLines = []
            let listIndices = []
            handle <- openFile dataPath ReadWriteMode
            contents <- hGetContents handle
            let listLines =  lines  contents
            let listOnlyFirstName = map (cutString ';') (lines contents)
            let personListString = map (map Text.unpack . Text.splitOn (Text.pack ";") . Text.pack)  listLines
            let personListStringName = map tail personListString
            let allNachname = map head personListStringName
            let listIndicesInsideNachname = map (`elemIndices` allNachname) lastNameList
            let listIndicesNachnameWithoutList =  head listIndicesInsideNachname
            putStrLn ("Der Index der Linie in File aller gesuchten Elemente: " ++ show  listIndicesNachnameWithoutList)
            let lengthResult = length listIndicesNachnameWithoutList
            putStrLn (show  lengthResult ++ " Suchergebnis ")
            printElements $ map (listLines !!) listIndicesNachnameWithoutList
            hClose handle
            putStrLn "Bitte geben Sie 'ENTER' ein um fortzufahren!"
            enter <- getLine
            main
        7 -> do 
            putStrLn "Programm wird beendet..."
        _ -> do
            putStrLn "Bitte wählen Sie eine Nummer von 1 bis 6!"
            main