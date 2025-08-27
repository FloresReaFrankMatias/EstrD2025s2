
{-
    1. Tip os recursivos simples
-}

-- 1.1. Celdas con b olitas
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}
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
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino


hayTesoro :: Camino -> Bool
hayTesoro Fin = False 
hayTesoro (Nada c) =  hayTesoro c   
hayTesoro (Cofre o c) = hayTesoroEnLosObjetos o || hayTesoro c


hayTesoroEnLosObjetos :: [Objeto] -> Bool
hayTesoroEnLosObjetos  []    = False
hayTesoroEnLosObjetos (o:os) = esTesoro o || hayTesoroEnLosObjetos os

esTesoro :: Objeto -> Bool 
esTesoro Tesoro  = True 
esTesoro _       = False  

pasosHastaTesoro :: Camino -> Int
--Precondición: tiene que haber al menos un tesoro
pasosHastaTesoro Fin = error "Tiene que haber al menos un tesoro"
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre objs c) = if hayTesoroEnLosObjetos objs
                                     then 0
                                     else  1 + pasosHastaTesoro c 


--alMenosNTesoros :: Int -> Camino -> Bool
--Indica si hay al menos n tesoros en el camino  


--alMenosNTesoros _ Fin      = False 
--alMenosNTesoros n (Nada c) = pasosHastaTesoro  c == n
--alMenosNTesoros n (Cofre objs c) = 

-------------------------------------------------------
{-
    2. Tipos arboreos
-}


data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

    deriving Show


sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT a  t1 t2) = a + sumarT t1  + sumarT t2 


sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT a t1 t2) = 1 +  sizeT t1 + sizeT t2 


mapDobleT :: Tree Int -> Tree Int
--Dado un árbol de enteros devuelve un árbol con el doble de cada número

mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n t1 t2) = NodeT (n*2) (mapDobleT  t2) (mapDobleT  t2)

