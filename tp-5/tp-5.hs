{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}

import Set 
import Queue  
import Stack
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
sinRepetidos' :: Eq a => [a] -> [a]
sinRepetidos' [] = []
sinRepetidos' (x:xs) = if pertenece x xs
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




 ----------------        SET   ------------------

--Costo :O(n)
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] _     = []
losQuePertenecen (x:xs) s = if belongs x s 
                            then x: losQuePertenecen xs s
                            else losQuePertenecen xs s     


--Costo: O()
sinRepetidos ::  Eq a => [a] -> [a]
sinRepetidos xs = setToList (sinRepetidosS xs)

--Costo: O()
sinRepetidosS :: Eq a => [a] -> Set a
sinRepetidosS []     = emptyS 
sinRepetidosS (x:xs) = addS x (sinRepetidosS xs)


data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT           = emptyS 
unirTodos (NodetT s t1 t2) = unirTodos(unionS (unirTodos t1) ( unirTodos t2))




---------------------     QUEUE             ------------------
  ---------------------------- INTERFAZ     ------------------------------
--Costo: O(1)
--emptyQ :: Queue a
  --Crea una cola vacía.

--Costo: O(1)
--isEmptyQ :: Queue a -> Bool
  --Dada una cola indica si la cola está vacía.

--Costo: O(n)
--enqueue :: a -> Queue a -> Queue a
   --Dados un elemento y una cola, agrega ese elemento a la cola.

--Costo: O(1)
--firstQ :: Queue a -> a
   --Dada una cola devuelve el primer elemento de la cola.

--Costo: O(1)
--dequeue :: Queue a -> Queue a
    --Dada una cola la devuelve sin su primer elemento
-------------------------------------------------------------

--Costo: O(n)
lengthQ :: Queue a -> Int
lengthQ q = if isEmptyQ q 
            then 0
            else 1 + (lengthQ (dequeue q)) 


--Costo: O(n^2)
queueToList ::  Queue a -> [a]
queueToList q = if isEmptyQ Q
                then []
                else firtsQ q : queueToList (dequeue q )


--Costo: O(n^2)
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if isEmptyQ q1
               then q2 
               else enqueue (firstQ q1) (unionQ (dequeue q1) q2 )


------------------------      STACK      --------------------------------------
--Costo: O(n)
apilar :: [a] -> Stack a
apilar []     = emptyS
apilar (x:xs) = push x (apilar xs)

--Costo: O(n)
desapilar :: Stack a -> [a]
desapilar s = if isEmptyS s 
              then []
              else top s : desapilar (pop s) 



