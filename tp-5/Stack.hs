   
module Stack (
       emptyS,
       isEmptyS,
       push,
       top,
       pop,
       lenS
)

where 


data Stack a = S [a] Int 
{-
    Inv Rep: en (S xs n) 
                n es la cantidad de elemento en  xs
-}


--Costo: O(1)
emptyS :: Stack
emptyS S = S []


--Costo: O(1)
isEmptyS :: Stack a -> Bool
isEmptyS (S xs n) = null xs

--Costo : O(1)
push :: a -> Stack a -> Stack a 
push x (S xs n) = (S (x:xs) (n+1))

--Costo: O(1)
pop :: Stack a -> Stack a 
pop (S xs n) = (S (tail xs) (n-1))


--Costo: O(1)
lenS :: Stack a -> Int
lenS (S xs n) = n
