
module PriorityQueue{
        emptyPQ,
        isEmptyQ,
        insertPQ,
        findMinPq,
        deleteMinPq
}

where 


data PriorityQueue a = PQ [a]


--Costo O(1)
emptyQ ::  PriorityQueue a 
--Devuelve una priority Queue vacia
emptyQ = PQ []

--Costo : O(1)
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ xs) = null xs 


--Costo : O(n)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a 
--Propósito: inserta un elemento en la priority queue.
insertPQ a (PQ xs) = PQ (insertarEnOrden a xs)


--Costo: O(n)
insertarEnOrden :: Ord a => a ->  [a] -> [ a]
insertarEnOrden x []     = [x]
insertarEnOrden x (y:ys) = if x < y 
                             then x : (y:ys)
                             else y : insertarEnOrden x ys 

--Costo : O(1)
findMinPq :: Ord a =>  PriorityQueue a -> a
findMinPq (PQ xs) = minimum  xs  

--Costo: O(n)
deleteMinPq :: Ord a => PriorityQueue a -> PriorityQueue a 
deleteMinPq (PQ xs) = PQ (borrarMin xs)


--Costo: O(n)
borrarMin :: Ord a => [a] -> [a]
borrarMin xs = borrar (minimum xs) xs 

--Costo: O(n)
borrar:: Ord a => a -> [a] -> [a]
borrar x []      = []
borrar x (y:ys)  = if x ==y
                   then ys
                   else y : borrar x xs 

