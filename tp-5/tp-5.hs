
{-
    1.CALCULO DE COSTOS
-}

--Costo: O(1)
head' :: [a] -> a
head' (x:xs) = x


--Csoto : Lineal O(n)
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

--Costo: O(n) Lineal
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1) --constante 



--Costo : = O(n) lineal 
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs -- constante

--Costo: = Lineal O()
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs
                     --constante

--Costo: = O()
pertenece :: Eq a => a -> [a] -> Bool
pertenece n []     = False
pertenece n (x:xs) = n == x || pertenece n xs --lineal


--Costo:
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                      then sinRepetidos xs
                      else x : sinRepetidos xs 
                      --constante


--Costo : O(n) lineal
-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys
                   --constante


--Costo: O(n) lineal
concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs
                    --constante
--Costo O(n) lineal
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs
                   --constante

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs


partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)


minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)


sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                 then xs
                 else x : sacar n xs


ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs = 
    let m = minimo xs
        in m : ordenar (sacar m xs)