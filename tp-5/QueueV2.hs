{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use foldr" #-}
module Queue (
    emptyQ,
    isEmptyQ,
    enqueue,
    firtsQ,
    dequeue,

)
where
data Queue a = Q [a]
{-

-}

--Costo: O(1)
emptyQ  :: Queue a
emptyQ = Q []

--Costo: O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs

--Costo : O(n)
enqueue :: a -> Queue a -> Queue a
enqueue x (Q ys) = Q (x:ys)  

-
--Costo: O(m)
firtsQ :: Queue a -> a 
firtsQ (Q xs) = ultElemento xs

--Costo : O(n) recorre cada elem hasta llegaral ultimo siendo n l cantidad de elem de xs
ultElemento :: [a] -> a
ultElemento (x:[]) = x
ultElemento (x:xs) = ultElemento xs    



--Costo: O(m)
dequeue :: Queue a -> Queue a 
dequeue (Q xs) = Q (sinElUltimoElemento xs)

--Costo : O(n)
sinElUltimoElemento :: [a] -> [a]
sinElUltimoElemento (x:[]) =
sinElUltimoElemento (x:xs) = x  : sinElUltimoElemento xs
