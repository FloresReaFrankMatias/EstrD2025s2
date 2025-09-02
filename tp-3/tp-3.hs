
{-
    1. Tip os recursivos simples
-}

-- 1.1. Celdas con b olitas
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia = 0
nroBolitas c (Bolita col ce ) = unoSiEsColor c col + nroBolitas c ce
celda1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
celda2 = Bolita Rojo (Bolita Azul CeldaVacia)
unoSiEsColor :: Color -> Color -> Int
unoSiEsColor Rojo Rojo = 1
unoSiEsColor Azul  Azul  = 1
unoSiEsColor _ _ = 0


poner :: Color -> Celda -> Celda
poner c celda = Bolita c celda

sonMismoColor :: Color -> Color ->Bool
sonMismoColor Rojo Rojo = True 
sonMismoColor Azul Azul = True
sonMismoColor _ _ = False

sacar :: Color -> Celda -> Celda
sacar c CeldaVacia       = CeldaVacia
sacar c (Bolita col cel) = if sonMismoColor c col
                            then cel 
                            else Bolita col (sacar c cel)

--Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.   
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 c cel = cel
ponerN n c celda = poner c (ponerN ( n - 1) c celda)  



{-
    1.2 CAMINO HACIA EL TESORO
-}

data Objeto = Cacharro | Tesoro
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show
-----------------------------------------
hayTesoro :: Camino -> Bool
hayTesoro Fin            = False 
hayTesoro (Nada cam)     =  hayTesoro cam   
hayTesoro (Cofre ob cam) = hayTesoroEnLosObjetos ob || hayTesoro cam


hayTesoroEnLosObjetos :: [Objeto] -> Bool
hayTesoroEnLosObjetos  []      = False
hayTesoroEnLosObjetos (ob:obs) = esTesoro ob || hayTesoroEnLosObjetos obs

esTesoro :: Objeto -> Bool 
esTesoro Tesoro  = True 
esTesoro _       = False  
-----------------------------------------
pasosHastaTesoro :: Camino -> Int
--Precondición: tiene que haber al menos un tesoro
pasosHastaTesoro Fin            = error "Tiene que haber al menos un tesoro"
pasosHastaTesoro (Nada c)       = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre objs c) = if hayTesoroEnLosObjetos objs
                                     then 0
                                     else  1 + pasosHastaTesoro c 

---------------------------------------
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n Fin             = False
hayTesoroEn n (Nada cam)      = hayTesoroEn (n-1) cam
hayTesoroEn n (Cofre obj cam) = if n == 0
                                   then hayTesoroEnLosObjetos obj
                                   else hayTesoroEn (n-1) cam


-----------------------------------
alMenosNTesoros  :: Int -> Camino -> Bool 
--Indica si hay al menos n tesoros en el camino  
alMenosNTesoros     n      c = cantTesorosDelCamino c >= n 

cantTesorosDelCamino :: Camino -> Int 
cantTesorosDelCamino  cam  = cantTesoros (objetosDeCamino cam)

objetosDeCamino :: Camino -> [Objeto] 
objetosDeCamino    (Cofre obs cam) = obs ++ objetosDeCamino cam 
objetosDeCamino    (Nada cam )     = objetosDeCamino cam 
objetosDeCamino    _               = []

cantTesoros :: [Objeto] -> Int
cantTesoros    []       = 0
cantTesoros    (ob:obs) = unoSi (esTesoro ob) + cantTesoros obs 

------------------------------------------
{-- Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
    el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
    incluidos tanto 3 como 5 en el resultado
--}
cantTesorosEntre :: Int -> Int -> Camino -> Int 
cantTesorosEntre    _ _ Fin           = 0
cantTesorosEntre   i f  (Nada cam)      = cantTesorosEntre (i - 1) (f - 1) cam 
cantTesorosEntre   i f  (Cofre ob cam )  = if i<= 1 && f >= 1
                                         then cantTesoros ob + cantTesorosEntre (i - 1) (f - 1) cam 
                                         else cantTesorosEntre (i - 1) (f - 1) cam 




-------------------------------------------------------
{-
    2. Tipos arboreos
-}


data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

--------------------------------------------------------------------    
ejemplo = NodeT (10 ::Int)
              (NodeT (5 :: Int) ( NodeT (5 :: Int) EmptyT EmptyT) EmptyT     ) 
              (NodeT (20 :: Int) (NodeT (12::Int)  EmptyT EmptyT) (NodeT (9::Int)  (NodeT (8::Int) EmptyT EmptyT) (NodeT (7::Int) EmptyT EmptyT)  ))

{-
                10
             /        \
           5         20
        /            /  \
        5           12   9
                   /  \ 
                   8   7             
-}

---------------------------------------------

{-
           "E"
         /    \
       "M"    "A"
        \     / \
        "O" "O" "M"
        /
        "S"
-}

ejemploStr =NodeT ("E" ::String)
              --Nodo Izq
              (NodeT ("M" :: String) ( NodeT ("O" :: String) (NodeT ("S"::String) EmptyT EmptyT) EmptyT) EmptyT  ) 
                --Nodo Der
              (NodeT ("A" :: String) (NodeT ("O"::String)  EmptyT EmptyT) (NodeT ("M"::String) EmptyT EmptyT ))
------------------------------------------------------
-- 2.1.1
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT a  t1 t2) = a + sumarT t1  + sumarT t2 

-- 2.1.2
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT a t1 t2) = 1 +  sizeT t1 + sizeT t2 

-- 2.1.3
mapDobleT :: Tree Int -> Tree Int
--Dado un árbol de enteros devuelve un árbol con el doble de cada número
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n t1 t2) = NodeT (n*2) (mapDobleT  t1) (mapDobleT  t2)

-- 2.1.4
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT e  EmptyT         = False
perteneceT e (NodeT x n1 n2) = e == x || perteneceT e n1 || perteneceT e n2 

-- 2.1.5
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT e EmptyT          = 0
aparicionesT e (NodeT x n1 n2) = unoSi(e==x) + aparicionesT e n1 + aparicionesT e n2

--Funcion del tp 2
unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

-- 2.1.6
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT x n1 n2)         = singularSi x (esHoja n1 n2)  ++ leaves n1 ++ leaves n2

singularSi :: a -> Bool -> [a]
singularSi x True  = [x]
singularSi x False = []

esHoja :: Tree a -> Tree a -> Bool
esHoja EmptyT EmptyT = True
esHoja _      _      = False


-- 2.1.7
heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT x n1 n2) = 1 + 1 + max (heightT n1) (heightT n2)



-- 2.1.8
mirrorT :: Tree a -> Tree a
mirrorT (NodeT x n1 n2 )  =  NodeT x n2 n1 

-- 2.1.9
toList :: Tree a -> [a]
toList  EmptyT         = []
toList (NodeT x n1 n2) = toList n1 ++ [x] ++ toList n2

-- 2.1.10
levelN :: Int -> Tree a -> [a]

levelN _ EmptyT         = []
levelN 0 (NodeT x n1 n2)= [x]
levelN n (NodeT x n1 n2)= levelN (n-1) n1 ++ levelN (n-1) n2

-- 2.1.11
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : juntarNiveles (listPerLevel t1) (listPerLevel t2)

juntarNiveles :: [[a]] -> [[a]]   -> [[a]]
juntarNiveles  []     yss        = yss
juntarNiveles  xss    []         = xss
juntarNiveles  (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

-- 2.1.12
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x n1 n2) = if heightT n1 > heightT n2
                                  then x : ramaMasLarga n1
                                  else x : ramaMasLarga n2                 

-- 2.1.13
todosLosCaminos :: Tree a         -> [[a]]
todosLosCaminos    EmptyT          = [   ]
todosLosCaminos    (NodeT x t1 t2) = [x] : consACada x (todosLosCaminos t1) ++ consACada x (todosLosCaminos t2)
  
consACada :: a -> [[a]]   -> [[a]]
consACada    x    [   ]    = [   ]
consACada    x    (xs:xss) = (x:xs) : consACada x xss 


-----------------------------------------------
{-
    2.2 Expresiones Aritmetricas
-}


data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA
    deriving Show

expresionA1 = Sum (Prod (Valor 2) (Valor 25) ) 
                 (Prod(Valor 1) (Neg  (Neg (Valor 5)) ) )
expresionA2 = Prod ( Sum (Valor 10) (Valor 10) )
                    (Prod  (Valor 5) (Valor 6)  )


--2.2.1
eval :: ExpA -> Int  
eval (Valor n)    = n
eval (Sum e1 e2)  = eval e1 + eval e2
eval (Prod e1 e2) = eval e1 * eval e2
eval (Neg e1 )    = eval e1 * (-1)

--2.2.2
--------------------------------
simplificar :: ExpA -> ExpA
simplificar (Valor n)    = Valor n
simplificar (Sum e1 e2)  = simplificarSum  (simplificar e1) (simplificar e2)
simplificar (Prod e1 e2) = simplificarProd (simplificar e1) (simplificar e2)
simplificar (Neg e1)     = simplificarNeg (simplificar e1)        


---------------------------------------
simplificarSum :: ExpA -> ExpA -> ExpA
simplificarSum (Valor 0 ) e         = e
simplificarSum e          (Valor 0) = e
simplificarSum e1         e2        = Sum e1 e2

---------------------------------------------
simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 0)  e         = Valor 0
simplificarProd  e         (Valor 0) = Valor 0

simplificarProd  e         (Valor 1) = e
simplificarProd (Valor 1)  e         = e

simplificarProd e1        e2        = Prod e1 e2

---------------------------------------
simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg e) = e
simplificarNeg e       = Neg e


