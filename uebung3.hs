-- Also grundsätzlich wird ein Wert für ein Intervall statt einem Key implementiert
-- Ergo brauchen wir erstmal einen Datentyp Intervall
data Intervall = Intervall [Integer]
-- Ein paar besteht immer aus eine Intervall und einem Wert
-- Wir haben aber natürlich nicht nur ein Intervall und einen Value, sondern beliebig viele
data IntervallMap k v = IntervallMap Intervall k v 

-- singleton erstellt eine Intervall Map mit einem Intervall von 0 - unendlich
singleton :: v -> IntervallMap k v 
singleton v = IntervallMap [0..] v

-- ord steht für Ordered. Wir machen das natürlich anhand von dem Wert k aka Intervall
-- Die Idee ist, dass wir das k in die beiden Werte zurückbringen und dann vergleichen,
-- ob der erste Wert von Wert a größer ist als der letzte Wert von b.
-- (!) :: Ord k => IntervalMap k v -> k -> v
 
-- da Datentypen nicht veränderbar sind, muss hier eine neue IntervallMap zurückgegeben werden
-- insert :: Ord k => k -> k -> v -> IntervalMap k v -> IntervalMap k v
-- insert x, y, v = IntervallMap (x, y) v

-- das wird benutzt, damit wir funktionen anwenden können auf eine IntervalMap
-- instance Functor (IntervalMap k) where

-- das wird benutzt um Operationen aneinanderzuhängen
-- instance Ord k => Applicative (IntervalMap k) where