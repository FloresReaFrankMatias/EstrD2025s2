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
--Costo: O(1)
emptyQ  :: Queue a
emptyQ = Q []

--Costo: O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs

--Costo : O(n)
enqueue :: a -> Queue a -> Queue a
enqueue x (Q ys) = Q (agregarAlFinal x ys)  

--Costo : O(1)
agregarAlFinal :: a -> [a] -> [a]
agregarAlFinal x []     = [x]
agregarAlFinal x (y:ys) = x: agregarAlFinal  x ys 

--Costo: O(1)
firtsQ :: Queue a -> a 
firtsQ (Q xs) = head xs


--sin head
--firtsQ (Q xs) = primerElemento xs 

--primerElemento :: [a] -> a 
--primerElemento (x:xs) = x

--Costo: O(1)
dequeue :: Queue a -> Queue a 
dequeue (Q xs) = Q (tail xs)
