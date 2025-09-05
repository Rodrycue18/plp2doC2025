
-- ej 2
cantDivisoresPrimos :: Int -> Int 
cantDivisoresPrimos 0 = 0
cantDivisoresPrimos 1 = 1
cantDivisoresPrimos n = length (filter (\y -> esPrimo(y) && (mod) n y == 0) (listaDecreciminento n))

esPrimo :: Int -> Bool
esPrimo 0 = False
esPrimo 1 = False
esPrimo x = revisoSiPrimo x (x-1)

revisoSiPrimo :: Int -> Int -> Bool
revisoSiPrimo  x 0 = True
revisoSiPrimo  x 1 = True
revisoSiPrimo  x y = not(x `mod` y == 0) && revisoSiPrimo x (y-1)

listaDecreciminento :: Int -> [Int]
listaDecreciminento 1 = []
listaDecreciminento x = x: listaDecreciminento (x-1)

-- EJ 3
data Maybes a = Nothings | Justs a
    deriving Show
data Eitherq a b = Leftq a | Rightq b 
    deriving Show

inverso :: Float -> Maybes Float
inverso x = if x /= 0 then Justs ((/) 1 x) else Nothings

--aEntero :: Eitherq Int Bool -> Int
--aEntero Leftq a b = if a == True then 1 else b
--aEntero Rightq x y = if b == False then 0 else 1
 
--ej 4
limpiar :: String -> String -> String
limpiar [] s = s 
limpiar (x:xs) y = limpiar xs (filter (\c -> c /= x) y)
-- Idenifique que la lista izquierda , es con la que tngo que jugar, es la que me da
-- el caso base , mi duda esta en la parte del filter como es que se renueva la parte del segundo y
-- duda resuelta , la parte del filter me devuelve mi nueva y , 


-- Queria hacer filter varias veces , pero no se como
--recursionFilter :: String -> String
--recursionFilter = foldr (filter (\c -> c /= x)) [] 

difPromedio :: [Float] -> [Float]
difPromedio xs = map (\x -> x - promedio xs (sumaDelista xs)) xs

sumaDelista :: [Float] -> Float
sumaDelista [] = 0
sumaDelista (x:xs) = x + sumaDelista xs

promedio ::  [Float] -> Float -> Float
promedio x y = y / (tamanio x ) 

tamanio :: [a]-> Float
tamanio [] = 0
tamanio (x:xs) = 1 + tamanio xs

-- no se que problema le dio con usar el maldito length

-- todos iguales
todosiguales :: [Int] -> Bool
todosiguales [] = True
todosiguales [x] = True
todosiguales (x:y:xs) = (x == y) && todosiguales xs  
-- no me gusto como lo resolvi , parece hardcodeado
