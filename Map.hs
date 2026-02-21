{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Map( 
        emptyM,
        assocM,
        lookupM,
        deleteM, 
        keys
)

where
data Map k v = M [(k,v)]

{-
        Costo Con rep:                Costo Sin rep:
        emptyM : O(1)                 emptyM : O(1)
        assocM: O(1)                  assocM:  O(k) 
        lookupM: O(K)                 lookupM: O(K)
        deleteM: O(k)                 deleteM: O(k)
        keys:   O(k*(2))              keys:    O(k)
-}



--Costo : O(1)
emptyM :: Map k v
emptyM = M []

-----------------------------------------------
--Costo: O(n)
assocM:: Eq k => k -> v -> Map k v -> Map k v  
assocM k v (M kvs) = M ( asociar k v kvs)

--Costo: O(n) por cada elemento realiza una ope constante(elem)
asociar :: Eq k => k -> v -> [(k,v)] ->[(k,v)]
asociar k v []             = [(k,v)]
asociar k v ((k',v'): kvs) = if k == k'
                             then (k',v'): kvs
                             else (k,v): asociar k v kvs           
-----------------------------------------------
--Costo: O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
--Propósito: encuentra un valor dado una clave.
lookupM k (M kvs) = buscarClaveEn k kvs 


--Costo: O(n)
buscarClaveEn :: Eq k=> k ->[(k,v)]-> Maybe v 
buscarClaveEn k []             = Nothing 
buscarClaveEn k ((k',v'): kvs) = if k == k'
                                 then Just v'
                                 else buscarClaveEn k kvs    
--------------------------------------
--Costo: O(n)
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M kvs) = M (deleteAssoc k kvs)

--Costo: O(n)
deleteAssoc ::Eq  k => k -> [(k,v)] -> [(k,v)]
deleteAssoc k  []        = []
deleteAssoc k  ((k',v') : kvs) = if k == k'
                           then kvs 
                           else (k',v') : deleteAssoc k kvs 
----------------------------
--costo: O(n)
keys :: Map k v -> [k]
keys (M kvs) = clavesDe kvs 


--costo:O(n) siendo n la cantidad de clave del map
       --    realiza una operacion constante por cada elemento
clavesDe ::[(k,v)] -> [k]
clavesDe []         = []
claveDe [(k,v):kvs] = k : clavesDe kvs 
