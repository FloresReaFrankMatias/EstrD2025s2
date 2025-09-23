module Queue (
    emptyQ,
    isEmptyQ,
    enqueue,
    firtsQ,
    dequeue,

)
where
data Queue a = Q [fs] [bs]
{-
  INV. REP: 
     -Si fs se encuentra vacia, la cola se encuentra vacia

-}

--Costo: O(1)
emptyQ :: Queue a 
emptyQ = Q [] []

--Costo: O(1)
isEmptyQ :: Queue a 
isEmptyQ (Q fs bs) = null fs




--Costo: O(n)
enqueue :: a -> Queue a -> Queue a 
enqueue x q = if isEmptyQ q 
                then agregarElemAFs x q
                else agregarElemABs x q

--Costo: O(1)
agregarElemAFs :: a -> Queue a -> Queue a 
agregarElemAFs x (Q fs bs) = Q (x:fs) bs


--Costo: O(1)
agregarElemABs :: a -> Queue a -> Queue a
agregarElemABs x (Q fs bs) = Q fs (x:bs)


--Costo: O(1)
firstQ :: Queue a 
firstQ (Q fs bs) = head fs 

--Costo: O(N): reverse e s lineal
dequeue ::  Queue a -> Queue a 
dequeue (Q fs bs) = if null (tail fs)
                    then Q (reverse bs) []
                    else Q (tail fs) bs

