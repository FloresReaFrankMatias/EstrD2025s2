{-
            1- PIZZAS

-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use list literal" #-}



data Pizza       = Prepizza      | Capa Ingrediente Pizza 
    deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show
-------------------Pizzas ejemplos--------------------------------
pizza0 = Prepizza
pizza1 = Capa Salsa Prepizza
pizza2 = Capa Queso (Capa Salsa Prepizza)
pizza3 = Capa (Aceitunas 8)
                           (Capa Queso (Capa Salsa Prepizza))
pizza4 = Capa Jamon (Capa Queso Prepizza)
-------------------------------------------------------
--1.1
cantidadDeCapas :: Pizza -> Int

cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p    

ingrs = [Salsa, Queso, Jamon]

--1.2
armarPizza :: [Ingrediente] -> Pizza
armarPizza  []  = Prepizza 
armarPizza  (i:is)  = Capa i (armarPizza is)


--
sacarJamon :: Pizza -> Pizza
sacarJamon  Prepizza  = Prepizza
sacarJamon (Capa i p) = if esJamon i
                        then sacarJamon p 
                        else Capa i (sacarJamon p) 

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False
                        
tieneSoloSalsaYQueso :: Pizza -> Bool 
tieneSoloSalsaYQueso Prepizza   = True
tieneSoloSalsaYQueso (Capa i p) = esSalsaOQueso i  && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Queso = True
esSalsaOQueso Salsa = True
esSalsaOQueso _     = False


duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza    = Prepizza
duplicarAceitunas (Capa i p ) = Capa (duplicarAceitunasSi i) 
                                       (duplicarAceitunas p) 
                                 
duplicarAceitunasSi :: Ingrediente -> Ingrediente
duplicarAceitunasSi (Aceitunas n) = Aceitunas (n*2)
duplicarAceitunasSi ing           = ing

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza [p]    = [(cantidadDeCapas p, p)]
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps
    
------------------------------------------------------------------------------

{-
    2- MAPAS DEL TESORO (CON BIFURCACIONES)
-}



data Dir = Izq | Der 
    deriving Show
data Objeto = Tesoro | Chatarra 
    deriving Show
data Cofre = Cofre [Objeto] 
    deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa 
    deriving Show
--------------------------------------------------

--2.1
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c)             = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnCofre c || hayTesoro m1 || hayTesoro m2


hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre objs) = hayTesoroEnObjetos objs

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos []     = False
hayTesoroEnObjetos (ob:obs) = esTesoro ob || hayTesoroEnObjetos obs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

--2.2
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn []     m                     = hayTesoroEnCofreDeMapa m
hayTesoroEn (d:ds) (Fin c)               = False
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) = if esIzq d
                                              then hayTesoroEn ds m1
                                              else hayTesoroEn ds m2 

hayTesoroEnCofreDeMapa :: Mapa -> Bool
hayTesoroEnCofreDeMapa (Fin c)             = hayTesoroEnCofre c
hayTesoroEnCofreDeMapa (Bifurcacion c _ _) = hayTesoroEnCofre c

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _        = False

--2.3 
caminoAlTesoro :: Mapa -> [Dir]
-- PRECOND: Existe un tesoro y es único.
caminoAlTesoro (Fin c)               = []
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoroEnCofre c
                                          then []
                                          else direccionAlMapaConTesoro m1 m2 : caminoAlTesoro (caminoConTesoroEntre m1 m2)

direccionAlMapaConTesoro :: Mapa -> Mapa -> Dir
-- PRECOND: Ninguna.
direccionAlMapaConTesoro m1 m2 = if hayTesoro m1
                                    then Izq
                                    else Der

caminoConTesoroEntre :: Mapa -> Mapa -> Mapa
-- PRECOND: Ninguna.
caminoConTesoroEntre m1 m2 = if hayTesoro m1
                                then m1
                                else m2

--2.4
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- PRECOND: Ninguna.
caminoDeLaRamaMasLarga (Fin c)               = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = if length (caminoDeLaRamaMasLarga m1) > length (caminoDeLaRamaMasLarga m2)
                                                  then Izq : caminoDeLaRamaMasLarga m1
                                                  else Der : caminoDeLaRamaMasLarga m2

--2.5 
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel   (Fin c)   = tesorosDeCofre c : []
tesorosPorNivel    (Bifurcacion c m1 m2 ) = tesorosDeCofre c : juntarNiveles (tesorosPorNivel m1) (tesorosPorNivel m2)

juntarNiveles :: [[a]] -> [[a]]   -> [[a]]
juntarNiveles    [   ]     yss     =  yss
juntarNiveles     xss     [   ]    =  xss
juntarNiveles    (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

tesorosDeCofre :: Cofre -> [Objeto]
tesorosDeCofre (Cofre o) = tesorosDeLista o 

tesorosDeLista :: [Objeto] -> [Objeto] 
tesorosDeLista []     = []
tesorosDeLista (o:os) = if esTesoro o
                            then o : tesorosDeLista os
                            else tesorosDeLista os

--2.6
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _)               = []
todosLosCaminos (Bifurcacion _ m1 m2) = consACada Izq (todosLosCaminos m1) ++ consACada Der (todosLosCaminos m2)

consACada :: a -> [[a]] -> [[a]]
consACada x []       = [[x]]
consACada x (ys:yss) = (x : ys) : consACada x yss  









---anotaciones  (Borrar despues)--------------



{----- Dispacher- se usa para darle mas escalabilidad al poder 
       tener multiples constructures, se usa en vez del doble pm

data gusto = chocolate |ddl | frutilla
 sonMiGusto :: Gusto-> Gusto -> bool
 sonMiGusto chocolate g = esChocolate g
 sonMiGusto ddl       g = esDdl g
 sonMiGusto frutilla  g = esfrutilla g


eschocholate :: Gusto ->  Bool
eschocholate  chocolate = True
eschocholate _          = False

esDdl :: Gusto ->  Bool
esDdl Ddl = True
esDdl _   = False

esFrutilla ::Gusto -> Bool
esFrutilla frutilla = True
esFrutilla  _  = False
-}





