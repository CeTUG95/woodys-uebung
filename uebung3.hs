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

-- To-Do: Stelle sicher, dass es nur für einen Intervall auch nur ein Intervall-Wert Paar gibt
insert :: Ord k => k -> k -> v -> IntervalMap k v -> IntervalMap k v
insert x y v (Singleton defaultValue) = IntervalMap [((Intervall x y), v)] defaultValue
insert x y v (IntervalMap xs defaultValue) = IntervalMap (xs ++ [((Intervall x y), v)]) defaultValue

-- das wird benutzt, damit wir funktionen anwenden können auf eine IntervalMap
-- instance Functor (IntervalMap k) where

-- das wird benutzt um Operationen aneinanderzuhängen
-- instance Ord k => Applicative (IntervalMap k) where