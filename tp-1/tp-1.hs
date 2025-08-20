

{-
    Ejercicio 2 Numero Enteros
-} 

-- 1 a) sucesor :: Int -> Int
sucesor :: Int -> Int
sucesor n =n +1
-- 1 b)
sumar :: Int -> Int -> Int
sumar n m = n +m 
--1 c)
divisionYResto :: Int -> Int ->(Int,Int)
--Precond: el parametro m no tiene que ser cero
divisionYResto n m = (  div n  m, mod n m)

--1 d)
maxDelPar :: (Int, Int) -> Int
maxDelPar (x, y) =
    if x >= y
       then x
       else y

-- 2.2

expresion1 = sucesor (maxDelPar (divisionYResto (sumar 8 1) 1))
expresion2 = sumar (maxDelPar (divisionYResto 20 (sucesor 2))) 4
expresion3 = maxDelPar (sucesor 9, sumar 0 (maxDelPar (divisionYResto 5 2)))
expresion4 = sumar (sucesor (maxDelPar (divisionYResto 21 3))) 2

{-
         3 Tipo Enumerativos
-}
--3 a)
data Dir = Norte | Sur  | Este | Oeste
    deriving Show
opuesto :: Dir -> Dir
opuesto d = 
    case d of
        Norte -> Sur
        Sur   -> Norte
        Este  -> Oeste
        Oeste -> Este
--3 b)
iguales :: Dir -> Dir -> Bool
iguales dir1 dir2=
    case (dir1,dir2) of
        (Norte,Norte)  -> True
        (Sur,Sur)      -> True
        (Este,Este)    -> False
        (Oeste, Oeste) -> False
        __             -> False
-- 3 c)
siguiente :: Dir -> Dir
--Precondicion : El parametro no puede ser Oeste
siguiente dir = 
    case dir of
        Norte -> Este
        Este  -> Sur
        Sur   -> Oeste
{-
    Es un funcion parcial porque tiene restriciones al momento de darle el argumento a dicha funcion
-}
--3.2
--a)
data DiaDeSemana = Lunes| Martes| Miercoles | Jueves| Viernes| Sabado | Domingo
    deriving Show
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDia, ultimoDia)
primerDia :: DiaDeSemana
primerDia = Lunes
ultimoDia :: DiaDeSemana
ultimoDia = Domingo

--b)
empiezaConM :: DiaDeSemana -> Bool
empiezaConM dia = case dia of
    Martes    -> True
    Miercoles -> True
    _         -> False
--c)

numeroDia :: DiaDeSemana -> Int
numeroDia Lunes     = 1
numeroDia Martes    = 2
numeroDia Miercoles = 3
numeroDia Jueves    = 4
numeroDia Viernes   = 5
numeroDia Sabado    = 6
numeroDia Domingo   = 7
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues dia1 dia2 = numeroDia dia1 >numeroDia dia2

--d)
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio d = case d of
    Lunes   -> False
    Domingo -> False
    _       -> True



--3.3
--a)
negar :: Bool -> Bool
negar False = True
negar _= False

--b)
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

--c)
yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _       = False

--d)
oBien :: Bool -> Bool -> Bool
oBien True _= True 
oBien _ True = True
oBien _ _ = False


{- 

            4 Registros

-}
--4.1
data Persona = P String Int deriving Show
nombre :: Persona -> String
nombre (P n _) = n 

edad :: Persona -> Int
edad (P _ e) = e


crecer :: Persona -> Persona
crecer (P n e)= P n (e+1) 

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nombre(P n e)= P nombre e 

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2) = e1 > e2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor (P n1 e1) (P n2 e2) = 
    if e1>e2
        then P n1 e1
        else P n2 e2

matias:: Persona
matias = P "Matias"  25
juan :: Persona
juan = P "Juan" 25


-- 4.2 
data TipoDePokemon =  Agua | Fuego | Planta 
        deriving Show
data Pokemon = Pokemon TipoDePokemon Int 
        deriving Show
data Entrenador = Entrenador String Pokemon Pokemon 
        deriving Show 
pokemon1 = Pokemon Agua 10
pokemon2 = Pokemon Agua 20
entr1 = Entrenador "Matias" pokemon1 pokemon2
entr2 = Entrenador "Matias" pokemon1 pokemon2
superaA :: Pokemon -> Pokemon -> Bool
superaA (Pokemon t _) (Pokemon t2 _) =
    case (t,t2) of
        (Agua, Fuego)   -> True
        (Fuego, Planta) -> True
        (Planta, Agua)  -> True
        _               -> False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipo (Entrenador _ p1 p2) =
    contarSiEsTipo tipo p1 + contarSiEsTipo tipo p2

contarSiEsTipo :: TipoDePokemon -> Pokemon -> Int
contarSiEsTipo Agua   (Pokemon Agua _)   = 1
contarSiEsTipo Fuego  (Pokemon Fuego _)  = 1
contarSiEsTipo Planta (Pokemon Planta _) = 1
contarSiEsTipo _      _                  = 0

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (Entrenador _ p1 p2,Entrenador _ p3 p4)= [p1,p2,p3,p4]

{-
     5 FUNCIONES POLIMORFICAS
-}
--5.1
-- a

loMismo :: a -> a
loMismo x = x

--b
siempreSiete :: a -> Int
siempreSiete x = 7

--c
swap :: (a,b) -> (b,a)
swap (x,y)=(y,x)
--5.2
{-
Estas funciones son  polimorficas porque pueden recibir argumentos de caulquier tipo
-}

{-
    6. Pattern Matching sobre Listas
-}

--6.2
estaVacia :: [a] ->Bool
estaVacia [] = True
estaVacia (_:_) = False    
--6.3
elPrimero :: [a] -> a
--Precondicion: la lista dada debe tener al menos un elemento
elPrimero (x:_) = x
--6.4
sinElPrimero ::[a] -> [a]
--Precondicion:La lista dada no debe ser vacia
sinElPrimero (_:xs) = xs
--6.5
--Precondicion:La lista dada no debe ser vacia
splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x, xs)
