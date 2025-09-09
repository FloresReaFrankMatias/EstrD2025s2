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

-----------------------------------------------------------------

{-
    3.NAVE ESPACIAL
-}



--------------------------------------------------

--3.1 
sectores :: Nave -> [SectorId]
sectores (N ts) = sectoresDeTreeSector ts

sectoresDeTreeSector :: Tree Sector -> [SectorId]
sectoresDeTreeSector EmptyT            = []
sectoresDeTreeSector (NodeT s ts1 ts2) = sectorIdDe s : sectoresDeTreeSector ts1 ++ sectoresDeTreeSector ts2

sectorIdDe :: Sector -> SectorId
sectorIdDe (S id _ _) = id

--3.2
--Proposito: Devuelve la suma de poder de propulsión de todos los motores de la nave.
-- Nota:el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion (N ts) = poderDePropulsionDeTs ts


poderDePropulsionDeTs :: Tree Sector -> Int
poderDePropulsionDeTs EmptyT          =  0
poderDePropulsionDeTs (NodeT s t1 t2) = poderDePropulsionDeSector s + poderDePropulsionDeTs t1 + poderDePropulsionDeTs t2

poderDePropulsionDeSector :: Sector -> Int 
poderDePropulsionDeSector (S _ cs _) = poderDePropulsionDeComponentes cs

poderDePropulsionDeComponentes :: [Componente] -> Int 
poderDePropulsionDeComponentes []     = 0
poderDePropulsionDeComponentes (c:cs) = poderDe c + poderDePropulsionDeComponentes cs  

poderDe :: Componente -> Int
poderDe    (Motor n) = n
poderDe     _        = 0   
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show

data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show

data Sector = S SectorId [Componente] [Tripulante]
    deriving Show

type SectorId = String
type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

data Nave = N (Tree Sector)
    deriving Show
--3.3
--Propósito: Devuelve todos los barriles de la nave

barriles :: Nave -> [Barril]
barriles (N ts) = barrilesDeTs ts

barrilesDeTs :: Tree Sector -> [Barril]
barrilesDeTs EmptyT          = [] 
barrilesDeTs (NodeT s t1 t2) = barrilesDeSector s ++ barrilesDeTs t1 ++ barrilesDeTs t2 

barrilesDeSector :: Sector -> [Barril]
barrilesDeSector   (S _ c t) = barrilesDeComponentes c 


barrilesDeComponentes :: [Componente] -> [Barril]
barrilesDeComponentes [] = []
barrilesDeComponentes    (c:cs) = barrilesDeComponente c ++ barrilesDeComponentes cs 

barrilesDeComponente :: Componente -> [Barril]
barrilesDeComponente     (Almacen bs) = bs
barrilesDeComponente     _            = []



--3.4
--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs id (N t) = N (agregarASectorT cs id t)

agregarASectorT :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorT _ _ EmptyT = EmptyT
agregarASectorT cs id (NodeT s izq der) = if sectorIdDe s == id
                                          then NodeT (agregarASectorActual cs s) izq der
                                          else NodeT s (agregarASectorT cs id izq) (agregarASectorT cs id der)

agregarASectorActual :: [Componente] -> Sector -> Sector
agregarASectorActual [] s = s
agregarASectorActual  cs1 (S id cs2 ts) = S id (cs1 ++ cs2) ts 


--3.5 
--Propósito: Incorp ora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA tr ids (N ts) = N (asignarTripulanteA' tr ids ts)

-- (Funcion auxiliar) Propósito: Incorpora un tripulante a una lista de sectores del árbol de sectores.
asignarTripulanteA' :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector 
asignarTripulanteA' _ _ EmptyT            = EmptyT
asignarTripulanteA' t sid (NodeT s t1 t2) = if (esSectorAAsignar s sid)
                                            then (NodeT (asignarTripulanteSector  t s) 
                                                        (asignarTripulanteA' t sid t1)
                                                        (asignarTripulanteA' t sid t2) 
                                                   )
                                            else (NodeT s (asignarTripulanteA' t sid t1) 
                                                          (asignarTripulanteA' t sid t2 )
                                                 )       


esSectorAAsignar :: Sector -> [SectorId] -> Bool 
esSectorAAsignar (S id _ _) ids = esSectorAAsignar' id ids

esSectorAAsignar' :: SectorId -> [SectorId] -> Bool
esSectorAAsignar' _    []      = False
esSectorAAsignar' sid (id:ids) = sid== id || esSectorAAsignar' sid ids


asignarTripulanteSector :: Tripulante ->  Sector
asignarTripulanteSector t (S id cs ts) = S id cs (t:ts)


--3.6

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
--Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados t (N ts) = sectoresAsignados' t ts

sectoresAsignados' :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignados' _ EmptyT          = []
sectoresAsignados' t (NodeT s t1 t2) = if estaAsignado t S
                                       then sectorIdDe s: sectoresAsignados' t t1 ++ sectoresAsignados' t t2
                                       else sectoresAsignados' t t1 ++ sectoresAsignados' t t2 




--3.7
tripulantes :: Nave -> [Tripulante]
tripulantes (N t) = tripulantesSinRepetir (tripulantesDeNave t)

tripulantesDeNave :: Tree Sector -> [Tripulante]
tripulantesDeNave EmptyT           = []
tripulantesDeNave (NodeT s t1 t2) = tripulantesDeSector s ++ 
                                         tripulantesDeNave t1 ++
                                        tripulantesDeNave t2

tripulantesDeSector :: Sector -> [Tripulante]
tripulantesDeSector (S _ _ tps) = tps


tripulantesSinRepetir :: [Tripulante] -> [Tripulante]
tripulantesSinRepetir      []      = []
tripulantesSinRepetir (tp:tps) = if   pertenece tp tps
                                 then tripulantesSinRepetir tps
                                 else tp : tripulantesSinRepetir tps



--funcion del tp2
pertenece :: Eq a => a -> [a] -> Bool
pertenece e []     = False
pertenece e (x:xs) = e == x || pertenece e xs 



----------------------------------------------------------------------

{-
    4-  MANADA DE LOBOS

-}



manada1= M ( Cazador "Simon" ["tito", "pepe"]
                 loboExp
                 cria1
                 cria2         
          )     

loboExp = Explorador "Predator" ["bosque", "rios"]
                      cria3
                      cria4

cria1 = "firu"
cria2 = "cria2"
cria3 = "cria3"
cria = "cria4"

-------------------------------------------------------------------------------


type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
            | Explorador Nombre [Territorio] Lobo Lobo
            | Cría Nombre
     deriving Show       
data Manada = M Lobo
     deriving Show


--4.2
buenaCaza :: Manada -> Bool
buenaCaza m  = alimentoCazado m > cantidadDeCrias m 

alimentoCazado :: Manada -> Int
alimentoCazado (M l) = alimentoCazado' l


alimentoCazado' :: Lobo -> Int
alimentoCazado' (Cria _)                = 0
alimentoCazado' (Explorador _ _ l1 l2)  = alimentoCazado' l1 + alimentoCazado' l2
alimentoCazado' (Cazador _ ps l1 l2 l3) = alimentoEn ps +
                                          alimentoCazado' l1 + 
                                          alimentoCazado' l2 +
                                          alimentoCazado' l3


alimentoEn :: [Presas] -> Int
alimentoEn ps = length ps

cantidadDeCrias :: Manada -> Int
cantidadDeCrias (M l) = cantidadDeCrias' l

cantidadDeCrias' :: Lobo -> Int
cantidadDeCrias' (Cria _)                 = 0
cantidadDeCrias' (Explorador _ _ l1 l2 )  = cantidadDeCrias' l1 
                                            + cantidadDeCrias' l2 
cantidadDeCrias' (Cazador _ _ _ l1 l2 l3) = cantidadDeCrias'l1 +
                                            cantidadDeCrias'l2 + 
                                            cantidadDeCrias'l3


esCria :: Lobo -> Bool
esCria Cria = True
esCria _    = False


---------------------------------
--4.3
{-
     Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
     con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
     cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
     cero presas
--}
elAlfa ::Manada -> (Nombre, Int)
elAfa (M l) = elAlfaDe l

elAlfaDe :: Lobo -> (Nombre,Int)
elAlfaDe (Cria n)                =
elAlfaDe (Explorador n _ l1 l2)  =
elAlfaDe (Cazador n ps l1 l2 l3) = elAlfaEntre ( elAlfaEntre (n (cantPresas ps))  elAlfaDe l1 )
                                               ( elAlfaEntre ( elAlfaDe l2 )     ( elAlfaDe l3) )

elAlfaEntre :: (Nombre ,Int) -> (Nombre ,Int) -> (Nombre ,Int)
elAlfaEntre   (nom1 , n1) (nom2, n2) = if n1 > n2
                                       then (nom1, n1)
                                       else (nom2, n2) 


cantPresas :: [Presa] -> Int
cantPresas []     = 0
cantPresas (p:ps) = 1 + cantPresas ps    


manadaEj =
Cazador "DienteFiloso" ["Búfalos", "Antílopes"] 
   (Cría "Hopito")
   (Explorador "Incansable" ["Oeste hasta el río"]
       (Cría "MechónGris")
       (Cría "Rabito")
   )
   (Cazador "Garras" ["Antílopes", "Ciervos"]
       (Explorador "Zarpado" ["Bosque este"]
           (Cría "Osado")
           (Cazador "Mandíbulas" ["Cerdos", "Pavos"]
               (Cría "Desgreñado")
               (Cría "Malcriado")
               (Cazador "TrituraHuesos" ["Conejos"]
                   (Cría "Peludo")
                   (Cría "Largo")
                   (Cría "Menudo")
               )
           )
        ) 
       (Cría "Garrita")
       (Cría "Manchas")
)
----------------------------
--4.4
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = losQueExploraronL t l


losQueExploraronL :: Territorio -> Lobo  -> [Nombre]
losQueExploraronL t (Cria _)                = []
losQueExploraronL t (Cazador _ _ l1 l2 l3)  = losQueExploraronL t l1 
                                              ++ losQueExploraronL t l2
                                              ++ losQueExploraronL t l3
losQueExploraronL t (Explorador n ts l1 l2) = if esTerritorioExplorado t ts
                                              then n : losQueExploraronL t l1 ++ losQueExploraronL t l2
                                              else losQueExploraronL t l1 ++ losQueExploraronL t l2
  

esTerritorioExplorado :: Territorio -> [Territorio] -> Bool 
esTerritorioExplorado _ []       = False
esTerritorioExplorado t (ts:tss) = t == ts || esTerritorioExplorado t tss 

----------------------------------
--4.5


-------------------------------
--4.6


















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





