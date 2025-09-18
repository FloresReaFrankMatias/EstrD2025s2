module Map {
        emptyM,
        assocM,
        lookupM,
        deleteM, 
        keys
}  

where
data Map k v = M [(k,v)]


--Costo : O(1)
emptyM :: Map k v
emptyM = M []


--Costo: O()
assocM:: Eq k => k -> v -> Map k v -> Map k v  
assocM k v (M kvs) = M ( asociar k v kvs)

asociar :: Eq k => k -> v -> Map k v
asociar








