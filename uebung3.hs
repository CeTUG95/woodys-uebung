-- Wir brauchen erstmal einen Datentyp Intervall
data Intervall startPoint endPoint = Intervall startPoint endPoint

-- Ein IntervalMap besteht aus vielen Intervall-Wert Paaren oder es ist ein Singleton
data IntervalMap k v = IntervalMap [(Intervall k k, v)] v | Singleton v

-- Hier initialsieren wir dann einfach unser Singleton
singleton :: v -> IntervalMap k v 
singleton v = Singleton v

-- Ord erstmal nur für Singleton / IntervalMap fehlt noch
(!) :: Ord k => IntervalMap k v -> k -> v
(!) (Singleton v) inputValue = v
(!) (IntervalMap [] defaultValue) inputValue = defaultValue 
(!) (IntervalMap ((Intervall a b, v) : xs) defaultValue) inputValue | inputValue >= a && inputValue < b = v 
																    | otherwise = (!) (IntervalMap xs defaultValue) inputValue

insert :: Ord k => k -> k -> v -> IntervalMap k v -> IntervalMap k v
insert x y v (Singleton defaultValue) = IntervalMap [((Intervall x y), v)] defaultValue
insert x y v (IntervalMap xs defaultValue) = IntervalMap ((remove x y xs) ++ [((Intervall x y ), v)]) defaultValue

remove :: Ord k => k -> k -> [(Intervall k k, v)] -> [(Intervall k k, v)]
remove x y [] = []
remove x y ((Intervall a b, v):xs) | a >= y && b >= y = remove x y xs ++ [(Intervall a b, v)] -- liegt nicht in der range
								   | a <= x && b <= x = remove x y xs ++ [(Intervall a b, v)] -- liegt nicht in der range
								   | x <= a && y >= b = remove x y xs 						  -- liegt innerhalb der range und wird entfernt					
                                   | a <= x && y >= b = remove x y xs ++ [(Intervall a x, v)] -- der anfang liegt nicht innerhalb in der range
								   | x <= a && b >= y = remove x y xs ++ [(Intervall y b, v)] -- das ende liegt nicht innerhalb in der range
								   | a <= x && b >= y = remove x y xs ++ [(Intervall a x, v)] ++ [(Intervall y b, v)] -- der anfang und das ende liegt nicht innerhalb in der range
								   
-- Bug: Ignoriert manche Werte und entscheidet sich für den Defaultwert
-- Wahrscheinlich darf ich hier nicht die Funkion auf f defaultValue anwenden. Aber ich bin mir nicht sicher wie ich das mache, weil er dann sich beschwert
instance Functor (IntervalMap k) where
	fmap f (Singleton v) = Singleton (f v)
	fmap f (IntervalMap liste defaultValue) = IntervalMap [((Intervall a b), (f v)) | ((Intervall a b), v) <- liste ] (f defaultValue)

-- Hab das hier gebruteforced, funktioniert irgendwie
instance Ord k => Applicative (IntervalMap k) where
 	pure f = Singleton (f)
	(<*>) (IntervalMap k v) b =  (fmap v b)
	

a = singleton 'a' :: IntervalMap Int Char
b = insert 10 20 'b' a
c = insert 9 21 'c' b
d = insert 5 15 'd' c
e = insert 14 22 'e' d
f = insert 10 19 'f' e
g = fmap fromEnum f
h = "Hello" <$ g
i = insert 5 10 110 $ insert 10 15 90 $ singleton 100 :: IntervalMap Int Int
j = insert 5 10 (-) $ insert 10 15 (*) $ singleton (+) :: IntervalMap Int (Int -> Int -> Int)
k = insert 3 18 2 $ singleton 10 :: IntervalMap Int Int
l = j <*> i <*> k -- Applikativer Funktor funktioniert noch nicht ganz

