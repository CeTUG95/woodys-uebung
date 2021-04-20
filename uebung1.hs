fizzBuzz :: (Integral a, Show a) => a -> [String]
fizzBuzz x = [returnFizzValue y | y <- [1..x]]

returnFizzValue :: (Integral a, Show a) => a -> String
returnFizzValue x   | mod x 3 == 0 && mod x 5 == 0 = "FizzBuzz"
                    | mod x 3 == 0 = "Fizz"
                    | mod x 5 == 0 = "Buzz"
                    | otherwise = show x
	
fibonacci :: (Eq a, Num a, Enum a) => [a]
fibonacci = [fibNumber y |y <- [1,2..]]

fibNumber :: (Eq a, Num a) => a -> a
fibNumber x | x == 1 = 0
            | x == 2 = 1
			| otherwise = fibNumber (x-1) + fibNumber (x-2)
			
primes :: (Integral a, Num a) => [a]
primes = [y | y <-[1..], isPrime y]
			
isPrime :: (Integral a, Num a) => a -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x | length [y | y <-[2..x-1], mod x y == 0] > 0 = False
		  | otherwise = True

primeFactor :: (Integral a, Num a) => a -> [a]
primeFactor x | x == 1 = []
			  | isPrime x = [x]
			  | otherwise = [head [y | y <- primes, mod x y == 0]] ++ primeFactor (div x (head [y | y <- primes, mod x y == 0]))


