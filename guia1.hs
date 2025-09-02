-- Nos pasamos directo a esquemas de recursion porque se ve mas interesante , pero volvere por los anteriores

-- Definicion de foldr, abstrae todo lo que se trate de recursion estructural, primero definamos recursion estructural;
-- La RECURSION ESTRUCTURAL TIENE 2 DETALLES QUE LA RESALTAN:
--      - El caso base devuelve cualquier cosa siempre y cuando la lista este vacia , o cualquier constructor de tipo represente el vacio
--      - La llamada recursiva se hace unicamente sobre la cola de la lista, sin vulnerar nada , y se puede hacer cualquier cosa con la cabeza 


-- foldr :: (a-> b-> b)-> b-> [a]-> b
-- foldr f z [] = z
-- foldr f z (x : xs) = f x (foldr f z xs)

-- EJEMPLO
-- foldr f z [x1,x2,x3]
-- -> f x1 (foldr f z [x2,x3]) 
-- -> f x1 (f x2 (foldr f z [x3]))
-- -> f x1 (f x2 (f x3 (foldr f z [] )))
-- -> f x1 (f x2 (f x3 (z)))

-- EJ 1
-- . Redefinir usando foldr las funciones sum, elem, (++), filter y map.

sum1 :: [Int] -> Int
sum1 = foldr (+) 0 

elem1 :: Eq a => a -> [a] -> Bool
elem1 y = foldr (\x rec -> (==) x y || rec) False

-- elem2 :: Eq a => a -> [a] -> Bool
-- elem2 _ [] = False
-- elem2 x (y:ys) = (==) x y || elem x ys 

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p = foldr (\x rec -> if p x then (x : rec) else rec) []

map1 :: (a -> b) -> [a] -> [b]
map1 f = foldr (\x rec-> (f x) : rec ) []

-- EJ 2
-- De nir la función mejorSegún :: (a-> a-> Bool)-> [a]-> a, que devuelve el máximo elemento
-- de la lista según una función de comparación, utilizando foldr1. Por ejemplo, maximum = mejorSegún (>).
