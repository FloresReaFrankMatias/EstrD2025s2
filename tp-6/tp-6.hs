 import PriorityQueue 
 import Map 





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







