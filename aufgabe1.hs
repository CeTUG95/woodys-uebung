-- Diese Aufgaben wurden mithilfe vom Code von Basti gelöst. Dennoch wurde die Funktion mit der Sytax verstanden. 
myhead :: [a] -> a
myhead (x:_) = x

mytail :: [a] -> [a]
mytail (_:x) = x

mylast :: [a] -> a
mylast (x:[]) = x
mylast (_:xs) = mylast xs

myinit :: [a] -> [a]
myinit [x] = []
myinit (x:xs) = x : myinit xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

fizzBuzz :: (Integral a, Show a) => a -> [String]
fizzBuzz x = [returnFizzBuzz y | y <- [1..x]]

returnFizzBuzz :: (Integral a, Show a) => a -> String
returnFizzBuzz x  | mod x 3 == 0 && mod x 5 == 0 = "FizzBuzz"
                  | mod x 3 == 0 = "Fizz"
                  | mod x 5 == 0 = "Buzz"
                  | otherwise = show x

fibonacci :: [Int]
fibonacci = [fibonumber y | y <- [1,2..]]

fibonumber :: Int -> Int
fibonumber x | x == 1 = 0
             | x == 2 = 1
             | otherwise = fibonumber (x-1) + fibonumber (x-2)

primes :: (Integral a, Num a) => [a]
primes = [y | y <-[1..], isPrime y]

isPrime :: (Integral a, Num a) => a -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x | length [y | y <-[2..x-1], mod x y == 0] > 0 = False
          | otherwise = True

primeFactors :: Integral a => a -> [a]
primeFactors x | x == 1 = []
               | isPrime x = [x]
               | otherwise = [head [y | y <- primes, mod x y == 0]] ++ primeFactors (div x (head [y | y <- primes, mod x y == 0]))
