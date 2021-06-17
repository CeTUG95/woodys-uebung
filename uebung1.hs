-- liefert den ersten Wert einer Liste
myhead :: [a] -> a
myhead (x:_) = x

-- liefert alle außer dem ersten Wert einer Liste
mytail :: [a] -> [a]
mytail (_:x) = x

-- liefert den letzten Wert einer LisreturnFe
mylast :: [a] -> a
mylast (x:[]) = x
mylast (_:xs) = mylast xs

-- liefert alle, bis auf den letzten Wert der Liste
myinit :: [a] -> [a]
myinit [x] = []
myinit (x:xs) = x : myinit xs

-- dreht die Elemente einer Liste um
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

-- fizzBuzz
fizzBuzz :: (Integral a, Show a) => a -> [String]
fizzBuzz x = [returnFizzBuzz y | y <- [1..x]]
-- Hilfsfunktion für Fizzbuzz
returnFizzBuzz :: (Integral a, Show a) => a -> String
returnFizzBuzz x  | mod x 3 == 0 && mod x 5 == 0 = "FizzBuzz"
                  | mod x 3 == 0 = "Fizz"
                  | mod x 5 == 0 = "Buzz"
                  | otherwise = show x

-- fibonacci
fibonacci :: [Int]
fibonacci = [fibonumber y | y <- [1,2..]]
-- Hilfsfunktion für fibonacci, die Fibonaccizahl für angegebenen Index angibt
fibonumber :: Int -> Int
fibonumber x | x == 1 = 0
             | x == 2 = 1
             | otherwise = fibonumber (x-1) + fibonumber (x-2)

-- primes
primes :: (Integral a, Num a) => [a]
primes = [y | y <-[1..], isPrime y]
-- Hilfsfunktion für Primes
isPrime :: (Integral a, Num a) => a -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x | length [y | y <-[2..x-1], mod x y == 0] > 0 = False
          | otherwise = True

-- primeFactors
primeFactors :: Integral a => a -> [a]
primeFactors x | x == 1 = []
               | isPrime x = [x]
--                | otherwise = [x] ++ [head [y | y <- primes, mod x y == 0]] ++ primeFactors (div x (head [y | y <- primes, mod x y == 0]))
               | otherwise = [head [y | y <- primes, mod x y == 0]] ++ primeFactors (div x (head [y | y <- primes, mod x y == 0]))