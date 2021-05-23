import Data.Char

-- some Caesar Chiffre with a bit help

isInRange :: Char -> Char -> Char -> Bool
isInRange l u c = (c >= l) && (c <= u)

-- Defined in Data.Char
--isLower :: Char -> Bool
--isLower = isInRange 'a' 'z'

--isUpper :: Char -> Bool
--isUpper = isInRange 'A' 'Z'

let2Int :: Char -> Char -> Int
let2Int x c = ord c - ord x

lcLet2Int :: Char -> Int
lcLet2Int = let2Int 'a'

ucLet2Int :: Char -> Int
ucLet2Int = let2Int 'A'

int2Let :: Char -> Int -> Char
int2Let x n = chr (ord x + n)

int2LcLet :: Int -> Char
int2LcLet = int2Let 'a'

int2UcLet :: Int -> Char
int2UcLet = int2Let 'A'

shift :: Int -> Char -> Char
shift n c | isLower c = int2LcLet ((lcLet2Int c + n) `mod` 26)
          | isUpper c = int2UcLet ((ucLet2Int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

decode :: Int -> String -> String
decode n = encode (-n)