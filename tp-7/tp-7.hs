
import MapSinRep 
import Set 

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show


belongBST :: Ord a => a -> Tree a -> Bool
--Prop: Dadao un BST dice si el elemento pertenece o no al arbol
--Costo: O(log n)
belongBST x EmptyT          = False
belongBST x (NodeT y ti td) = (x == y) || (if x < y
                                           then  belongBST x ti
                                           else  belongBST x td)

insertBST :: Ord a => a -> Tree a -> Tree a
--Prop: Dado un BST inserta un elemento en el arbol
--Costo: O(log n)
insertBST  x EmptyT         = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = if x == y
                              then NodeT x ti td
                              else if x < y
                                   then NodeT y (insertBST x ti) td
                                   else NodeT y ti (insertBST x td)



deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST x EmptyT          = EmptyT
deleteBST x (NodeT y ti td) = if x == y
                              then rearmarBST  ti td
                              else if x < y
                                   then NodeT y (deleteBST x ti) td
                                   else NodeT y ti (deleteBST x td)

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
 -- PRECOND: ambos árboles son BSTs
rearmarBST EmptyT td = td
rearmarBST ti td = NodeT (maxBST ti) (delMaxBST ti) td

maxBST:: Ord a=> Tree a -> a 
-- PRECOND: no es vacío
maxBST (NodeT x _ EmptyT) = x 
maxBST (NodeT _ _ td)     = maxBST td

delMaxBST:: Ord a=> Tree a -> Tree a 
-- PRECOND: no es vacío
delMaxBST (NodeT _ ti EmptyT) = ti 
delMaxBST (NodeT x ti td) = NodeT x ti (delMaxBST td)

splitMinBST :: Ord a => Tree a -> (a, Tree a)
--Propósito : dado un BST devuelve un par con el mínimo elemento y el árb ol sin el mismo.
--Costo : O(log N )

splitMinBST (NodeT x  EmptyT td ) = (x, td)  
splitMinBST (NodeT x ti td)     = let (m, ti') = splitMinBST ti
                                   in (m, NodeT x ti td)

splitMaxBST :: Ord a => Tree a -> (a, Tree a)
  -- PRECOND: el árbol es BST, y NO está vacío
splitMaxBST (NodeT x ti EmptyT) = (x, ti)  
splitMaxBST (NodeT x ti td)     = let (m, td') = splitMaxBST td
                                   in (m, NodeT x ti td')

esBST :: Tree a -> Bool
--Propósito : indica si el árbol cumple con los invariantes de BST.
--Costo : O(N^2) donde n es la cantidad de elementos del Tree  y 
 --               se realiza operaciones cosntantes por cada elemento
esBST EmptyT          = True
esBST (NodeT x ti td) = esMenorQue ti x && 
                        esMayorQue td x &&  
                        esBST ti && 
                        esBST td 


esMenorQue :: Ord a => Tree a -> a -> Bool
--Costo : O(1)
esMenorQue  EmptyT         x = True 
esMenorQue (NodeT y ti td) x = y < x 

esMayorQue :: Ord a=> Tree  a -> a -> Bool
--Costo : O(1)
esMayorQue  EmptyT         x = True 
esMayorQue (NodeT y ti td) x = y > x  

----------
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA y EmptyT                  = Nothing
elMaximoMenorA y (NodeT x EmptyT EmptyT) = if y > x 
                                           then Just y 
                                           else Nothing     

elMaximoMenorA y (NodeT x ti td)         = if  y < x
                                           then elMaximoMenorA y ti 
                                           else elMaximoMenorA y td

elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
--Propósito : dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
--elemento dado.
--Costo : O(log N ) donde N es la cantidad de elementos de arbol
elMinimoMayorA x EmptyT                  = Nothing
elMinimoMayorA x (NodeT y EmptyT EmptyT) = if x < y
                                           then Just y 
                                           else Nothing


elMinimoMayorA x (NodeT y ti td )        = if x < y 
                                           then   elMinimoMayorA x td
                                           else   elMinimoMayorA x ti           



balanceado :: Tree a -> Bool
--Propósito : indica si el árb ol está balanceado. Un árb ol está balanceado cuando para cada
--nodo la diferencia de alturas entre el subarb ol izquierdo y el derecho es menor o igual a 1.
--Costo : O(N 2)
balanceado EmptyT          = True 
balanceado (NodeT x ti td) = abs(heightT ti - heightT td) <= 1


--Funcion de la practica 3
heightT :: Tree a -> Int
--Costo: O(N^2) realiza dos recursiones sobre cada nodos
heightT EmptyT          = 0
heightT (NodeT x n1 n2) = 1 +  max (heightT n1) (heightT n2)  




------------------------------------ Ejercicio 4 Empresa   ------------------------------------

type SectorId = Int
type CUIL = Int
data Empresa = ConsE (Map SectorId (Set Empleado)) 
                     (Map CUIL Empleado)

consEmpresa :: Empresa
--Propósito : construye  una empresa vacía
--Costo : O(1)
consEmpresa = ConsE emptyM emptyM

buscarPorCUIL :: CUIL -> Empresa -> Empleado
--Propósito : devuelve el empleado con dicho CUIL.
--Precondición : el CUIL es de un empleado de la empresa.
--Costo : O(log E) siendo E la cantidad de empleados de la empresa
buscarPorCUIL c (Cons m1 m2) = lookup c m2 

empleadosDelSector :: SectorId -> Empresa -> [Empleado]
--Propósito : indica los empleados que traba jan en un sector dado.
--Costo : O(log S + E) siendo S la cantidad de sectores y E la cantida de empleados
empleadosDelSector s (ConsE m1 m2) = let empleadosEnSector = lookup s m1
                                      in setToList empleadosEnSector 


todosLosCUIL :: Empresa -> [CUIL]
--Propósito : indica to dos los CUIL de empleados de la empresa.
--Costo : O(E) siendo E la cantidad  de empleados de la empresa
todosLosCUIL (ConsE m1 m2) = keys m2  

todosLosSectores :: Empresa -> [SectorId]
--Propósito : indica to dos los sectores de la empresa.
--Costo : O(S)
todosLosSectores (ConsE m1 m2) = keys m1

agregarSector :: SectorId -> Empresa -> Empresa
--Propósito : agrega un sector a la empresa, inicialmente sin empleados.
--Costo : O(log S)
agregarSector sId (ConsE m1 m2) = ConsE ( assocM sId emptyS m1) m2   

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
--Propósito : agrega un empleado a la empresa, que traba jará en dichos sectores y tendrá el
--            CUIL dado.
--Costo : calcular

agregarEmpleado ss c (ConsE m1 m2) = ConsE ()  ()


borrarEmpleado :: CUIL -> Empresa -> Empresa
--Propósito : elimina al empleado que posee dicho CUIL.
--Costo : calcular
borrarEmpleado c (ConsE m1 m2) = let empAEliminar = lookup c m2 
                                  in ConsE (actSectorSiEmppleado m1 empAEliminar) ( deleteM  empAEliminar m2)

actSectorSiEmppleado :: Set Empleado -> Set Empleado 

