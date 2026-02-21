{-
    Inv
    -en la heap no puede haber dos tuplas con la misma segunda componente
    - los productos de la heap deben estar presentes en los nodos ancestros con una cantidad 
       igual o mayor a la del nodo actual
    -   en la heap no peude haber tuplas con la misma priemr componenete negativa
    
-}
import Set

import Map

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}


type Categoria = String
type CaminoJ = [Categoria]
type Producto = String


data MVTree = MVT (Heap (Int,Producto) )  (Map Categoria MVTree) deriving Show




--Costo: O(J^ 2  por inits 
--          +
 --       J * (J log SC + P log P) por masVendidoEn ya que  el camino mas largo tiene la misma longitud de al lista de caminos 
--         
--
-- => O(J^ 2 + J* (J log SC + J + P log P)
esMasVendido :: Producto -> CaminoJ -> Int -> MVTree -> Bool
esMasVendido p cs n t = esMasVendido' p (inits  cs) n t 


esMasVendido' :: Producto -> [CaminoJ] -> Int -> MVTree -> Bool
{-  Costo: N* por recursion sbre la lista de caminos
            (J log SC + P log P) + P por usus de masVendidoEn
            seindo J la longitud del camino mas largo en la lista
            y elem ya que la lista resultante no puede ser mayor
              longitud a la cant de productos 

  N * (J log SC + P log P)
-}

esMasVendido' p [] n t     = false
esMasVendido' p (cs:css) n t = elem p (masVendidoEn n cs t  ) || esMasVendido' p css n t  --comtempla lista vacia el inits 



--intis. N * N




{-              INTERFAZ   -}
--Costo: O(1) por emprtyH y y emptyM
empTyMVTree :: MVTree
empTyMVTree = MVT empTyH empTyM


{-Costo : O(J  * #reusion por la jerarquia 
                (log SC)) por assocM  de map sobre categoria
 


-}

registrarCategoria::CaminoJ -> MVTree ->  MVTree
registrarCategoria [] t              = t
registrarCategoria (c:cs) (MVT h m ) = case lookUpM c m of 
                                        Nothing -> MVT h (assocM c (registrarCategoria cs ( MVT emptyH emptym  )) m)                
                                        Just t -> MVT h (assocM c  (registrarCategoria cs t) m)




{-
    Costo: O(J * #recursion por la jerarquia 
                  log Sc # por lookup sobre map
             )
             + SC por keysM     



-}
subCategoriasDe :: CaminoJ -> MVTree -> [Categoria]
subCategoriasDe [] (MVT h m) = keysM m
subCategoriasDe (c:cs) (MVT h m) = case lookUpM c m of 
                        Nothing -> error "cateogria invalida"
                        Just t  -> subCategoriasDe cs t


{-Costo
 O( J * #recursion sobre la lista de cateogrias
                  log Sc # por lookup sobre map de sub categorias
             )
             + P * log P  por registrarVentaH en la heap

-}

registrarVenta :: Producto -> CaminoJ  -> MVTree -> MVTree
registrarVenta  p [] (MVT h m) = MVTree (registrarVentaH p h) m
registrarVenta p (c:cs) (MVT h m) = case lookUpM c m of 
                        Nothing -> error "jerarquia invalida"
                        Just t  -> MVT  (   )




{- Costo:  O(P  * # siendo p la cant de elem  de la heap y recursion sobre la misma
              log P) #por insert y deleteMaxH

-}

registrarVentaH :: Producto -> Heap(Int,Producto)  -> Heap(Int,Producto)
registrarVentaH p h = if isEmptyH h
                     then insert (1,p) h 
                     else let (i,p')= findMaxH h in 
                          if p == p' 
                          then  insert (i+1,p) (deleteMaxH h)
                          else insert (i,p) ( registrarVentaEn p (deleteMaxH h) )

{- Costo: O(P * log P) por insert y deleteMaxH

O( J           # porr recursion sobre lsitas de categorias
   log SC      #  por lookup sobre el map de categorias
    + P log P) # por takeH en la Heap
-}

masVendidoEn :: Int -> CaminoJ -> MVTree -> [Producto]
masVendidoEn i [] (MVT h m)     = if i < 0
                                  then error "cantidad invalida"
                                  else take i h  
masVendidoEn i (c:cs) (MVT h m) =case lookUpM c m of 
                                   Nothing -> error "jerarquia invalida"
                                   Just t  -> masVendidoEn cs t




{-
    Costo: O(P *  # siendo P la cant de elem de la heap por RE sobre la misma
                 log P)  por deleteMaxH en cada instancia de la recursion
-}
takeH :: Int -> Heap (Int,Producto) -> [Producto]

takeH 0 h = []
takeH n h =  if isEmptyH h 
            then []
            else  snd (findMaxH h) : takeH (n-1) (deleteMaxH h)










