 import PriorityQueue 
 import Map 



------------------ USUARIO MAP   --------------------



{-     COSTOS DE LA INTERFAZ
       emptyM   ---->  O(1)
       assocM   ---->  O(n)
       lookupM  ---->  O(n)
       deleteM  ---->  O(n)
       keys     ---->  O(n)


-}

--Costo : O(n^2)
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m  = valuesDeKeys (keys m) m 


--Costo : O(n^2) por cada elemento realiza operaciones 
              -- de costo constantes
valuesDeKeys :: Eq k => [k]   -> [(k, v)]  -> [Maybe v]
valuesDeKeys []     m =
valuesDeKeys (k:ks) m = lookupM k m : valuesDeKeys ks m      
----


--Costo:O(n^2) por cada elemnto realiza una operacion  lookupM(lineal)
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas []     m = True
todasAsociadas (k:ks) m = isJust (lookupM k m) && todasAsociadas ks m

-----
--Costo: O(n^2) por cada elemnto realiza op de consto lineal
       --       siendo n la cantidad de claves en el map
--
listToMap :: Eq k => [(k, v)] -> Map k v
--Propósito: convierte una lista de pares clave valor en un map.
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)



-------
--Costo: O()
mapToList :: Eq k => Map k v -> [(k, v)]
--Propósito: convierte un map en una lista de pares clave valor
mapToList (M )


------

agruparEq :: Eq k => [(k, v)] -> Map k [v]
--Costo: O(t*m) siendo t los elemento

--just: por cada elemento de la lista se realiza operacion
       -- lineal sobre el map
agruparEq [] =
agruparEq ((x,y):ts) =
               let resto = agruparEq ts 
               in  case lookupM x resto of
                   Nothing -> assocM x [y] resto
                   Just ys -> assocM x (y:ys) resto   

--con rep
--costo: O()




