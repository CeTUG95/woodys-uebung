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
(!) (IntervalMap ((Intervall a b, v) : xs) defaultValue) inputValue | inputValue >= a && inputValue <= b = v 
																    | otherwise = (!) (IntervalMap xs defaultValue) inputValue

insert :: Ord k => k -> k -> v -> IntervalMap k v -> IntervalMap k v
insert x y v (Singleton defaultValue) = IntervalMap [((Intervall x y), v)] defaultValue
insert x y v (IntervalMap xs defaultValue) = IntervalMap ((remove x y xs) ++ [((Intervall x y), v)]) defaultValue

-- To-Do: Ist off bei einem Wert
remove :: Ord k => k -> k -> [(Intervall k k, v)] -> [(Intervall k k, v)]
remove x y [] = []
remove x y ((Intervall a b, v):xs) | x <= a && y >= b = remove x y xs 						-- liegt innerhalb der range und wird entfernt					
                                   | a <= x && y >= b = remove x y xs ++ [(Intervall a x, v)] -- der anfang liegt nicht innerhalb in der range
								   | x <= a && b >= y = remove x y xs ++ [(Intervall y b, v)] -- das ende liegt nicht innerhalb in der range
								   | a <= x && b >= y = remove x y xs ++ [(Intervall a x, v)] ++ [(Intervall y b, v)] -- der anfang und das ende liegt nicht innerhalb in der range
								   
-- das wird benutzt, damit wir funktionen anwenden können auf eine IntervalMap
-- Hier gibt es einen Case, der fehlt...
instance Functor (IntervalMap k) where
	fmap f (Singleton v) = Singleton (f v)
	fmap f (IntervalMap [((Intervall a b), v)] defaultValue) = IntervalMap [((Intervall a b), (f v))] (f defaultValue)
	
-- das wird benutzt um Operationen aneinanderzuhängen
-- instance Ord k => Applicative (IntervalMap k) where
-- 	pure f = f k 
--	(IntervalMap k) f g <*> = IntervalMap [((Intervall a b), (f g v))] (f g defaultValue)