module MapConRep {
        emptyM,
        assocM,
        lookupM,
        deleteM, 
        keys
}  

where
data Map k v = M [k] [v]




--Costo : O(1)
emptyM :: Map k v
emptyM = M []

-----------------------------------------------
--Costo: O(1)
assocM:: Eq k => k -> v -> Map k v -> Map k v  
assocM k v (M ks vs) = M (k:ks) (v:vs)          
-----------------------------------------------
--Costo: O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
--Propósito: encuentra un valor dado una clave.
lookupM k (M ks vs) = buscarClaveEn k ks vs 


--Costo: O(n)
buscarClaveEn :: Eq k=> k ->[k]->[v]-> Maybe v 
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
deleteAssoc k  [(k',v')] = if k = k'
                           then kvs 
                           else (k',v) : deleteAssoc k kvs 
----------------------------
--costo: O(n)
keys :: Map k v -> [k]
keys (M kvs) = sinRepetidos(clavesDe kvs) 


--costo:O(n) siendo n la cantidad de clave del map
       --    realiza una operacion constante por cada elemento
clavesDe ::[(k,v)] -> [k]
clavesDe []         = []
claveDe [(k,v):kvs] = k : clavesDe kvs 


--Costo : O(n^2) siendo n la cantidad de lementos de la lista 
--             donde por cada elmento realiza una op constante
sinRepetidos ::[k] ->[k]
sinRepetidos []     = []
sinRepetidos (k:ks) = if elem k ks
                      then sinRepetidos ks
                      else k : sinRepetidos ks

