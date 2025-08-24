
{-
    1-Recursión sobre listas
-}
-- 1.1 
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

--1.2
longitud :: [a] -> Int
longitud [] = 0
longitud (n:ns) = 1 + longitud ns

-- 1.3
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n:ns) = n+1 :  sucesores ns

--1.4
conjuncion :: [Bool] -> Bool
conjuncion []     = True
conjuncion (b:bs) = b && conjuncion bs

--1.5
disyuncion :: [Bool] -> Bool
disyuncion []     = True
disyuncion (b:bs) = b || disyuncion bs

--1.6
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

--1.7
pertenece :: Eq a => a -> [a] -> Bool
pertenece e []     = False
pertenece e (x:xs) = e == x || pertenece e xs 

--1.8
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) = unoSi (esDelMismoElemento e x) + apariciones e xs

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

esDelMismoElemento :: Eq a => a -> a -> Bool
esDelMismoElemento x y = x == y

--1.9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []       = []
losMenoresA n (ns:nss) = listaDeNumeroSiSinoNil ns (ns < n) n ++ losMenoresA n nss

listaDeNumeroSiSinoNil :: Int -> Bool -> Int -> [Int]
listaDeNumeroSiSinoNil ns True  n = [ns]
listaDeNumeroSiSinoNil ns False n = []

--1.10
{-Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
de n elementos -}
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n []     = [[]]
lasDeLongitudMayorA n (x:xs) = listaDeElementoSiSinoNil x (longitud x > n) ++ lasDeLongitudMayorA n xs

listaDeElementoSiSinoNil :: [a] -> Bool -> [[a]]
listaDeElementoSiSinoNil x True  = [x]
listaDeElementoSiSinoNil x False = []

--1.11
{-Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
lista.
-}
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] y     =[y] 
agregarAlFinal (x:xs) y = x: agregarAlFinal xs y

--1.12
{-  Dadas dos listas devuelve la lista con to dos los elementos de la primera lista y todos los
elementos de la segunda a continuación. Definida en Haskell como (++).
-}
agregar :: [a] -> [a] -> [a]
agregar [] ys     = ys
agregar (x:xs) ys = x : agregar xs ys

--1.13
{-
    Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
    en Haskell como reverse
-}
reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = agregar (reversa xs) [x]

--1.14
{-
    Dadas dos listas de enteros, devuelve una lista donde el elemento en la p osición n es el
    máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
    las listas no necesariamente tienen la misma longitud.
-}
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos []     ys     = ys
zipMaximos xs     []     = xs
zipMaximos (x:xs) (y:ys) = primerNumeroSiSinoElSegundo x (x >= y) y : zipMaximos xs ys

primerNumeroSiSinoElSegundo :: Int -> Bool -> Int -> Int
-- PRECOND: Ninguna.
primerNumeroSiSinoElSegundo n True  m = n
primerNumeroSiSinoElSegundo n False m = m

--1.15 Dada una lista devuelve el mínimo

elMinimo :: Ord a => [a] -> a
elMinimo [x]    = x
elMinimo (x:xs) = min x (elMinimo xs)

{-
    2 - Resursión sobre numeros
-}

--2.1
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial(x-1)

--2.2
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n >= 1
                       then n : cuentaRegresiva (n-1)
                       else [] 

--2.3
repetir :: Int -> a -> [a]
repetir n e = if n >=1
              then e: repetir (n-1) e
              else []

--2.4
{-
    Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
    Si la lista es vacía, devuelve una lista vacía 
-}
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _     =  []
losPrimeros _ []     = [] 
losPrimeros n (x:xs) = x : losPrimeros(n-1) xs

--2.5 
{-
    Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
    recibida. Si n es cero, devuelve la lista completa. 
-}
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs     = xs
sinLosPrimeros _ []     = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs


