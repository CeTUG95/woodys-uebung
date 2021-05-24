isSpace :: Char -> Bool
isSpace c = c == ' '

isLowerAlphabeticChar :: Char -> Bool
isLowerAlphabeticChar c = (fromEnum c >= fromEnum 'a') && (fromEnum c <= fromEnum 'z')

isUpperAlphabeticChar :: Char -> Bool
isUpperAlphabeticChar c = (fromEnum c >= fromEnum 'A') && (fromEnum c <= fromEnum 'Z')

addWhitespaceEveryFiveCharacters :: [Char] -> [Char]
addWhitespaceEveryFiveCharacters xs | length xs <= 5 = xs
                                    | otherwise = take 5 xs ++ " " ++ addWhitespaceEveryFiveCharacters (drop 5 xs)
									
encryptCaesar :: Int -> [Char] -> [Char]
encryptCaesar n message = addWhitespaceEveryFiveCharacters $ map (\c -> calculateEncription n c) $ filter (\c -> not (isSpace c)) message

calculateEncription :: Int -> Char -> Char
calculateEncription n c | isUpperAlphabeticChar c = toEnum (fromEnum 'A' + (mod ((fromEnum c - fromEnum 'A') + n) 26))::Char
                        | isLowerAlphabeticChar c = toEnum (fromEnum 'a' + (mod ((fromEnum c - fromEnum 'a') + n) 26))::Char
                        | otherwise = c

decryptCaesar :: Int -> [Char] -> [Char]
decryptCaesar n message = map (\c -> calculateDecription n c) $ filter (\c -> not (isSpace c)) message

calculateDecription :: Int -> Char -> Char
calculateDecription n c | isUpperAlphabeticChar c = toEnum (fromEnum 'A' + (mod ((fromEnum c - fromEnum 'A') - n) 26))::Char
						| isLowerAlphabeticChar c = toEnum (fromEnum 'a' + (mod ((fromEnum c - fromEnum 'a') - n) 26))::Char
						| otherwise = c

repeatString:: [Char] -> Int -> [Char]
repeatString xs n | length xs > n = take n xs 
				  | length xs < n = xs ++ (repeatString xs (n - length xs))
				  
charToInt:: Char -> Int
charToInt c | isUpperAlphabeticChar c = (fromEnum c - fromEnum 'A')
			| isLowerAlphabeticChar c = (fromEnum c - fromEnum 'a')
			
encryptVigenere:: [Char] -> [Char] -> [Char]
encryptVigenere key message = addWhitespaceEveryFiveCharacters $ map (\(n,c) -> calculateEncriptionVigenere n c) $ zip (repeatString key (length message)) $ filter (\c -> not (isSpace c)) message

calculateEncriptionVigenere :: Char -> Char -> Char
calculateEncriptionVigenere n c | (isUpperAlphabeticChar c) || (isLowerAlphabeticChar c) = toEnum (fromEnum 'a' + (mod (charToInt c + charToInt n) 26))::Char
                                | otherwise = c

decryptVigenere:: [Char] -> [Char] -> [Char]
decryptVigenere key message = map (\(n,c) -> calculateDecriptionVigenere n c) $ zip (repeatString key (length message)) $ filter (\c -> not (isSpace c)) message

calculateDecriptionVigenere :: Char -> Char -> Char
calculateDecriptionVigenere n c | (isUpperAlphabeticChar c) || (isLowerAlphabeticChar c) = toEnum (fromEnum 'a' + (mod (charToInt c - charToInt n) 26))::Char
                                | otherwise = c