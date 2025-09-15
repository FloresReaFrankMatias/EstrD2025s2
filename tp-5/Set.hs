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

data Set a= Set [a] Int    


emptyS :: Set a
emptyS = Set [] 0


addS :: Eq a => a -> Set a -> Set a
add  x (Set [] n) = Set x n
addS x (Set ys n) = if elem x  ys
                    then Set ys n
                    else Set (x:ys) (n+1)


belongs :: Eq a => a -> Set a -> Bool
belongs x (Set ys _) =  elem x ys


sizeS ::Eq a => Set a -> Int
sizeS (Set xs n) = n

removeS :: Eq a => a -> Set a -> Set a
removeS x (Set ys n) = 
    if  elem x ys
     then Set (removeS' x ys) (n-1)
     else error "El elemento no esta en el conjunto dado"

removeS' :: Eq a => a -> [a] -> [a]
removeS' x (y:ys) = 
    if x == y
     then ys
     else y : removeS' x ys

unionS :: Eq a => Set a -> Set a -> Set a
unionS (Set xs _) s2 = unionS' xs s2

unionS' :: Eq a => [a] -> Set a -> Set a
unionS' [] s      = s
unionS' (x:xs) ys = addS x (unionS' xs ys) 

setToList ::Eq a => Set a -> [a]
setToList (Set xs _) = xs






