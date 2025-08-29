-- iniciamos guia 0
-- EJ1

-- Dar el tipo y describir el comportamiento de las siguientes-
--  funciones del módulo Prelude de Haskell:
-- null head tail init last drop take (++) concat reverse elem

-- --------------------------------------------
-- null revisa si la lista esta vacia ejm null [] = true , null [1] = false
-- head saca el primer elem de una lista, tail es una sublista de la lista original sin el primer elem
-- init saca el ultimo elemento de la lista init [1,2,3] = [1,2] DEVUELVE LA LISTA MODIFICADA
-- last devuelve el ultimo elemento de una lista last [1,2,3] = 3 
-- drop toma un int y una lista donde el int es una cantidad de elementos a sacar por delante de la lista
-- drop 2 [1,2,3] = [3]
-- take recibe un int y una lista y te devulve una sublista con la cantidad especificada en el int 
-- take 3 [1,2,3,4] = [1,2,3]
-- (++) concatena 2 listas , recibe 2 listas y devulve la union de esas listas
-- concat es una aplanadora de sublistas, concat [[1,2,3],[6,7,8],[4,5,7]] = [1,2,3,6,7,8,4,5,7]
-- reverse da la vuelta la lista reverse [1,2,3] = [3,2,1]
-- elem es la funcion pertenece , revisa si un valor esta dentro de una lista

--Ej 2 

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

-- data Either a b = Left a | Right b 
--     deriving Show

inverso :: Float -> Maybes Float
inverso x = if x /= 0 then Justs ((/) 1 x) else Nothings

aEntero :: Either Int Bool -> Int
aEntero (Left a) = a 
aEntero (Right a) = if a == True then 1 else 0
 
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

-- ej 5
data AB a = Nil | Bin (AB a) a (AB a)
    deriving Show

vacioAB :: AB a -> Bool -- verifica si un arbol no tiene nodos
vacioAB Nil = False
vacioAB (Bin i r d) = True
-- vacioAB _ = True

-- Bin (Nil) Nil (Nil) 
preorder :: AB a -> [a]
preorder Nil = []
preorder (Bin i r d) = [r] ++ preorder i ++ preorder d
-- Esta funcion quiero que agrege un nodo a la izquierda
-- Idea: Cuando llego al ULTIMO nodo hijo a la izquierda es donde quiero agregar el nodo nuevo
-- Caso sencillo agregar nil a la izquierda, mi idea es recorrer hasta el ultimo nodo hijo izq y ahi agregar
-- Un problema , al recorrer hasta el ultimo nodo izquierdo , mi arbol se destruye
-- Como evito la destruccion de los nodos de arriba , al momento de recorrer el arbol?
 
agregarNodoIzq :: AB a -> AB a -> AB a
agregarNodoIzq Nil Nil = Nil
agregarNodoIzq (Bin i r d) Nil = if defIgualdad i Nil then (Bin (Nil) r d) else agregarNodoIzq  i Nil 

-- viajeAUltimoNodoIzq
--     i == Nil
defIgualdad :: AB a -> AB a-> Bool
defIgualdad Nil Nil = True
defIgualdad a b = False
