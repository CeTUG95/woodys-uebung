isLowerAlphabeticChar :: Char -> Bool
isLowerAlphabeticChar c = (fromEnum c >= fromEnum 'a') && (fromEnum c <= fromEnum 'z')

isUpperAlphabeticChar :: Char -> Bool
isUpperAlphabeticChar c = (fromEnum c >= fromEnum 'A') && (fromEnum c <= fromEnum 'Z')

alphabetShift :: Char -> Int -> Char
alphabetShift x n = toEnum (fromEnum x + n)::Char

lowerAlphabetShift :: Int -> Char
lowerAlphabetShift = alphabetShift 'a'

upperAlphabetShift :: Int -> Char
upperAlphabetShift = alphabetShift 'A'


isSpace :: Char -> Bool
isSpace c = c == ' '

removeSpaces :: [Char] -> [Char]
removeSpaces (xs) = filter (/=' ') xs

addSpace :: [Char] -> [Char]
addSpace xs | length xs <= 5 = xs
            | otherwise = take 5 xs ++ " " ++ addSpace (drop 5 xs)

encryptCaesar :: Int -> [Char] -> [Char]
encryptCaesar _ [] = []
encryptCaesar n (x:xs) | isSpace x = encryptCaesar n xs
                       | isLowerAlphabeticChar x = lowerAlphabetShift (fromEnum x + n) `mod` 26) : encryptCaesar n xs
                       | isUpperAlphabeticChar x = upperAlphabetShift (fromEnum x + n) `mod` 26) : encryptCaesar n xs
                       -- | otherwise



decryptCaesar :: Int -> [Char] -> [Char]
decryptCaesar _ [] = []
