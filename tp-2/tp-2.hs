
{-
    1-Recursión sobre listas
-}
-- 1.1 

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

--1.2
longitud :: [a] -> Int
longitud [] = 0
longitud (n:ns) = 1 + longitud ns

-- 1.3
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n:ns) = n+1 :  sucesores ns

--1.4
conjuncion :: [Bool] -> Bool
conjuncion []     = True
conjuncion (b:bs) = b && conjuncion bs

--1.5
disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (b:bs) = b || disyuncion bs

--1.6
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

--1.7
pertenece :: Eq a => a -> [a] -> Bool
pertenece e []     = False
pertenece e (x:xs) = e == x || pertenece e xs 

--1.8
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) = unoSi (esDelMismoElemento e x) + apariciones e xs

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

esDelMismoElemento :: Eq a => a -> a -> Bool
esDelMismoElemento x y = x == y

--1.9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []       = []
losMenoresA n (ns:nss) = listaDeNumeroSiSinoNil ns (ns < n) n ++ losMenoresA n nss

listaDeNumeroSiSinoNil :: Int -> Bool -> Int -> [Int]
listaDeNumeroSiSinoNil ns True  n = [ns]
listaDeNumeroSiSinoNil ns False n = []

--1.10
{-Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
de n elementos -}
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n []     = []
lasDeLongitudMayorA n (x:xs) = if ( (longitud x) > n  )
                               then x : lasDeLongitudMayorA n xs
                               else lasDeLongitudMayorA n xs

--1.11
{-Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
lista.
-}
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] y     =[y] 
agregarAlFinal (x:xs) y = x: agregarAlFinal xs y

--1.12
{-  Dadas dos listas devuelve la lista con to dos los elementos de la primera lista y todos los
elementos de la segunda a continuación. Definida en Haskell como (++).
-}
agregar :: [a] -> [a] -> [a]
agregar [] ys     = ys
agregar (x:xs) ys = x : agregar xs ys

--1.13
{-
    Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
    en Haskell como reverse
-}
reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = agregar (reversa xs) [x]

--1.14
{-
    Dadas dos listas de enteros, devuelve una lista donde el elemento en la p osición n es el
    máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
    las listas no necesariamente tienen la misma longitud.
-}
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos []     ys     = ys
zipMaximos xs     []     = xs
zipMaximos (x:xs) (y:ys) = primerNumeroSiSinoElSegundo x (x >= y) y : zipMaximos xs ys

primerNumeroSiSinoElSegundo :: Int -> Bool -> Int -> Int
-- PRECOND: Ninguna.
primerNumeroSiSinoElSegundo n True  m = n
primerNumeroSiSinoElSegundo n False m = m

--1.15 Dada una lista devuelve el mínimo

elMinimo :: Ord a => [a] -> a
elMinimo [x]    = x
elMinimo (x:xs) = min x (elMinimo xs)

{-
    2 - Resursión sobre numeros
-}

--2.1
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial(x-1)

--2.2
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n >= 1
                       then n : cuentaRegresiva (n-1)
                       else [] 

--2.3
repetir :: Int -> a -> [a]
repetir n e = if n >=1
              then e: repetir (n-1) e
              else []

--2.4
{-
    Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
    Si la lista es vacía, devuelve una lista vacía 
-}
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _     =  []
losPrimeros _ []     = [] 
losPrimeros n (x:xs) = x : losPrimeros(n-1) xs

--2.5 
{-
    Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
    recibida. Si n es cero, devuelve la lista completa. 
-}
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs     = xs
sinLosPrimeros _ []     = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs

{-
    3. Registros
-}
--3.1 Personas
--------------------------------------------
data Persona = P String Int
    deriving Show

per1= P "matias" 20
per2 = P "juan" 22

--------------------------
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA 0 _      = []
mayoresA _ []     = []
mayoresA n (x:xs) = listaDePersonaSiSinoNil x (edad x > n) ++ mayoresA n xs

listaDePersonaSiSinoNil :: Persona -> Bool -> [Persona]
listaDePersonaSiSinoNil p True  = [p]
listaDePersonaSiSinoNil p False = []


{-
    Dada una lista de p ersonas devuelve el promedio de edad entre esas p ersonas. Precon-
    dición : la lista al menos p osee una p ersona.
-}
promedioEdad :: [Persona] -> Int
promedioEdad xs = div (sumatoriaDeEdades xs) (longitud xs)

sumatoriaDeEdades :: [Persona] -> Int
sumatoriaDeEdades []     = 0
sumatoriaDeEdades (x:xs) = edad x + sumatoriaDeEdades xs

edad :: Persona -> Int
edad (P _ e)= e


{-
    Dada una lista de p ersonas devuelve la p ersona más vieja de la lista. Precondición : la
    lista al menos p osee una p ersona
-}
elMasViejo :: [Persona] -> Persona
--Precondicion: La lsita debe tener al menos una persona
elMasViejo [p]    = p
elMasViejo (p:ps) = if edad p > edadDe (elMasViejo ps)
                    then p
                    else elMasViejo ps


edadDe :: Persona -> Int
edadDe (P _ e) = e

--3.2 Pokemones
---------------------------------------
data TipoDePokemon = Agua | Fuego | Planta
        deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int
        deriving Show
data Entrenador = ConsEntrenador String [Pokemon]
        deriving Show
-----------------------------------------
--Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon :: Entrenador -> Int
cantPokemon ( ConsEntrenador n p) = sumatoriaDePokemones p

sumatoriaDePokemones :: [Pokemon] -> Int
sumatoriaDePokemones []     = 0
sumatoriaDePokemones (p:ps) = 1 + sumatoriaDePokemones ps


cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
-- Devuelve la cantidad de Pokémon de determinado tip o que p osee el entrenador
cantPokemonDe t (ConsEntrenador n p) = sumatoriaDePokemonesDe t p

sumatoriaDePokemonesDe :: TipoDePokemon -> [Pokemon] -> Int
sumatoriaDePokemonesDe t []     = 0
sumatoriaDePokemonesDe t (p:ps) =  unoSiPokemonEsTipoCeroSino t p + sumatoriaDePokemonesDe t ps

unoSiPokemonEsTipoCeroSino :: TipoDePokemon -> Pokemon -> Int
unoSiPokemonEsTipoCeroSino t (ConsPokemon tp e)  = unoSiTipoEsMismoTipoQue t tp

unoSiTipoEsMismoTipoQue :: TipoDePokemon -> TipoDePokemon -> Int
unoSiTipoEsMismoTipoQue Agua   Agua   = 1
unoSiTipoEsMismoTipoQue Fuego  Fuego  = 1
unoSiTipoEsMismoTipoQue Planta Planta = 1
unoSiTipoEsMismoTipoQue _      _      = 0
{-
    Dados dos entrenadores, indica la cantidad de Pokemon de cierto tip o pertenecientes al
    primer entrenador, que le ganarían a todos los Pokemon del segundo entrenador
-}
cuantosDeTipo_De_LeGananATodosLosDe_:: TipoDePokemon -> Entrenador -> Entrenador -> Int

cuantosDeTipo_De_LeGananATodosLosDe_  t (ConsEntrenador _ ps1) (ConsEntrenador _ ps2) = pokemonsGanadoresDeTipoEntre t ps1 ps2

pokemonsGanadoresDeTipoEntre :: TipoDePokemon ->[Pokemon] -> [Pokemon] -> Int
pokemonsGanadoresDeTipoEntre t _      []  = 0
pokemonsGanadoresDeTipoEntre t []     _   = 0
pokemonsGanadoresDeTipoEntre t (p:ps) ps2 = unoSi ( esMismoTipo t (tipoPokemonDe p) && superaATodos p ps2  ) 
                                                       + pokemonsGanadoresDeTipoEntre t ps ps2



superaATodos :: Pokemon -> [Pokemon] -> Bool
superaATodos p  []      =  True
superaATodos p (p2:ps2) = superaA p p2 && superaATodos p ps2

--Funcion del tp-1
superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPokemon t _) (ConsPokemon t2 _) =
    case (t,t2) of
        (Agua, Fuego)   -> True
        (Fuego, Planta) -> True
        (Planta, Agua)  -> True
        _               -> False
{-
    Dado un entrenador, devuelve True si p osee al menos un Pokémon de cada tipo posible.
-}

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ ps) = hayUnoDeCadaTipo ps


hayUnoDeCadaTipo :: [Pokemon] -> Bool
hayUnoDeCadaTipo [ ] = False
hayUnoDeCadaTipo ps = tienePokemonTipo Agua ps && tienePokemonTipo Fuego  ps && tienePokemonTipo Planta ps

tienePokemonTipo :: TipoDePokemon -> [Pokemon] ->Bool
tienePokemonTipo t []      = False
tienePokemonTipo t (p:ps ) = esMismoTipo t (tipoPokemonDe p) || tienePokemonTipo t ps

--Funcion de la practica 1
esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Fuego Fuego   = True
esMismoTipo Planta Planta = True
esMismoTipo Agua Agua     = True
esMismoTipo _ _           = False

tipoPokemonDe :: Pokemon -> TipoDePokemon
tipoPokemonDe (ConsPokemon t _) = t


----------------------------------------------------------------------------------------
--3.3 Roles
----------------------------------------------------
data Seniority = Junior | SemiSenior | Senior
        deriving Show
data Proyecto = ConsProyecto String
        deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
        deriving Show
data Empresa = ConsEmpresa [Rol]
        deriving Show
--------------------------------------------------
{-
    Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
-}
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = proyectosDeDiferentesRoles rs
--Dado una lista de roles describe la lista de proyectos sin repetir
proyectosDeDiferentesRoles :: [Rol] -> [Proyecto]
proyectosDeDiferentesRoles [] =[]
proyectosDeDiferentesRoles (r:rs) = agregarProyectoSiNoExiste (proyecto r) (proyectosDeDiferentesRoles rs)

--Dado un rol describe su proyecto
proyecto :: Rol -> Proyecto
proyecto (Developer _ p1)  = p1
proyecto (Management _ p2) = p2

--Dado un proyecto y uan lista de proyecto describe una lista de proyectos diferentes
agregarProyectoSiNoExiste :: Proyecto -> [Proyecto]-> [Proyecto]
agregarProyectoSiNoExiste p [] = [p]
agregarProyectoSiNoExiste p (pr:prs) =
    if esElMismoProyecto p pr
        then pr : prs
        else pr : agregarProyectoSiNoExiste p prs 
--Dado 2 proyectos describe si son iguales
esElMismoProyecto :: Proyecto ->Proyecto -> Bool 
esElMismoProyecto (ConsProyecto p) (ConsProyecto p2)= p == p2
--------

{-
    Dada una empresa indica la cantidad de desarrolladores senior que p osee, que p ertecen
    además a los proyectos dados por parámetro
-}

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) ps  = cantidadDeDevSeniorEn rs ps 

--Dado una lista de roles y de proyectos denota cantidad de dev seniors
cantidadDeDevSeniorEn :: [Rol] ->[Proyecto] -> Int
cantidadDeDevSeniorEn []     ps = 0

cantidadDeDevSeniorEn (r:rs) ps = unoSi( esDevSeniorEnAlgunProyecto r ps) + cantidadDeDevSeniorEn rs ps


esDevSeniorEnAlgunProyecto :: Rol -> [Proyecto] -> Bool
esDevSeniorEnAlgunProyecto r ps = esSenior (seniorityDe r) && perteneceAAlgunProyecto r ps 

--Dado un rol denota su seniority
seniorityDe :: Rol -> Seniority
seniorityDe (Developer s _)  = s
seniorityDe (Management s _) = s

--Dado un senority denota si es Senior
esSenior :: Seniority -> Bool 
esSenior Senior = True
esSenior _ = False

--Dado un rol y una lista de proyectos denota si el rol pertenece a alguno de los proyectos de la lista dada
perteneceAAlgunProyecto :: Rol -> [Proyecto] -> Bool
perteneceAAlgunProyecto  r []    = False
perteneceAAlgunProyecto r (p:ps) = esElMismoProyecto (proyecto r) p || perteneceAAlgunProyecto r ps



{-
    Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
-}
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (ConsEmpresa rs) = cantDeEmpleadosQueTrabajanEn ps rs

cantDeEmpleadosQueTrabajanEn :: [Proyecto]-> [Rol] -> Int
cantDeEmpleadosQueTrabajanEn _ [] = 0
cantDeEmpleadosQueTrabajanEn [] _ = 0
cantDeEmpleadosQueTrabajanEn ps (r:rs) = unoSi (perteneceAAlgunProyecto  r ps) + cantDeEmpleadosQueTrabajanEn ps rs  


{-
    Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
    cantidad de personas involucradas.
-}

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = asignadosPorProyecto' rs


asignadosPorProyecto' :: [Rol] -> [(Proyecto, Int)]
asignadosPorProyecto' []     = []
asignadosPorProyecto' (r:rs) = agregarProyectoATuplas (proyectoDeRol r) (asignadosPorProyecto' rs)

agregarProyectoATuplas :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)]
agregarProyectoATuplas p []           = [(p, 1)]
agregarProyectoATuplas p ((p', n):ts) = 
    if esElMismoProyecto p p'
        then (p', n+1):ts
        else (p',n) : agregarProyectoATuplas p ts


proyectoDeRol ::  Rol ->  Proyecto
proyectoDeRol (Developer _ p) = p
proyectoDeRol (Management _ p) = p


