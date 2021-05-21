#To-Do: Implementiere, dass es auch als Buchstaben dargestellt wird
#To-Do: Implementiere isAlpha um nur Buchstaben zu verschlüsseln
#To-Do: Kein Leerzeichen am Ende von der Nachricht

encryptCaesar :: Int -> [Char] -> [Char]
encryptCaesar n message = addWhitespaceEveryFiveCharacters $ map (\c -> toEnum (mod ((fromEnum c) + n) 26)) $ filter (\c -> not (isSpace c)) message

isSpace :: Char -> Bool
isSpace c = c == ' '

addWhitespaceEveryFiveCharacters :: [Char] -> [Char]
addWhitespaceEveryFiveCharacters [] = []
addWhitespaceEveryFiveCharacters (x:[]) = [x]
addWhitespaceEveryFiveCharacters message = take 5 message ++ " " ++ addWhitespaceEveryFiveCharacters (drop 5 message)

decryptCaesar :: Int -> [Char] -> [Char]
decryptCaesar n message = map (\c -> toEnum (mod ((fromEnum c) - n) 26)) $ filter (\c -> not (isSpace c)) message