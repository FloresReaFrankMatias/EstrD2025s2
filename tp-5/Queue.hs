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

emptyQ  :: Queue a
emptyQ = Q []


isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs


enqueue :: a -> Queue a -> Queue a
enqueue x (Q ys) = Q (agregarAlFinal x ys)  


agregarAlFinal :: a -> [a] -> [a]
agregarAlFinal x []     = [x]
agregarAlFinal x (y:ys) = x: agregarAlFinal  x ys 


firtsQ :: Queue a -> a 
firtsQ (Q xs) = head xs


--sin head
--firtsQ (Q xs) = primerElemento xs 

--primerElemento :: [a] -> a 
--primerElemento (x:xs) = x


dequeue :: Queue a -> Queue a 
dequeue (Q xs) = Q (tail xs)
