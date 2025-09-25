module Map {
        emptyM,
        assocM,
        lookupM,
        deleteM, 
        keys
}  

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


--Costo: O()
assocM:: Eq k => k -> v -> Map k v -> Map k v  
assocM k v (M kvs) = M ( asociar k v kvs)

asociar :: Eq k => k -> v -> [(k,v)] ->[(k,v)]
asociar k v []             = [(k,v)]
asociar k v [(k',v': kvs)] = if k == k'
                             then (k',v'): kvs
                             else (k,v): asociar           








