{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use infix" #-}



module Set(
    Set,
    emptyS,
    addS,
    belongs,
    sizeS,
    removeS,
    unionS,
    setToList 
)
where 

data Set a= Set [a]     
{-
    Sin INV REP

-}

--Costo: O(1)
emptyS :: Set a
emptyS = Set [] 

--Costo: O(1)
addS :: Eq a => a -> Set a -> Set a
addS x (Set ys ) = (S (x:ys))

--Costo: O(n)
belongs :: Eq a => a -> Set a -> Bool
belongs x (Set ys _) =  elem x ys

---Costo: O(n)
sizeS ::Eq a => Set a -> Int
sizeS (Set xs) = length xs


--Costo: O(n)
removeS :: Eq a => a -> Set a -> Set a
removeS x (Set ys n) =(S (removeS' x ys) )

--Costo: O(n)
removeS' :: Eq a => a -> [a] -> [a]
removeS' x (y:ys) = 
    if x == y
     then ys
     else y : removeS' x ys

--Costo: O(n*m)
unionS :: Eq a => Set a -> Set a -> Set a
unionS (Set xs ) s2 =  (S (xs++ys))



--Costo: O(n)
setToList ::Eq a => Set a -> [a]
setToList (Set xs ) = sinRepetidos xs


--Costo =O(n (2))
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos []     = []
sinRepetidos (x:xs) = if elem x xs
                      then sinRepetidos xs
                      else x: (sinRepetidos xs)      




