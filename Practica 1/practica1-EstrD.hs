

------ Ejercicio 2 Numero Enteros

-- 1 a) sucesor :: Int -> Int
sucesor :: Int -> Int
sucesor n =n +1
-- 1 b)
sumar :: Int -> Int -> Int
sumar n m = n +m 
--1 c)
divisionYResto :: Int -> Int ->(Int,Int)
--Precond: m no tiene que ser cero
divisionYResto n m = (  div n  m, mod n m)

--1 d)
maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = max n m

-- 2
{-
sucesor (maxDelPar (divisionYResto (sumar 8 1) 1))
sumar (maxDelPar (divisionYResto 20 (sucesor 2))) 4
maxDelPar (sucesor 9, sumar 0 (maxDelPar (divisionYResto 5 2)))
sumar (sucesor (maxDelPar (divisionYResto 21 3))) 2
-}
--3 Tipo Enumerativos

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
        (Norte,Norte) -> True
        (Sur,Sur)-> True
        (Este,Este) -> False
        (Oeste, Oeste) -> False
        __ -> False
siguiente :: Dir -> Dir
siguiente dir = 
    case dir of
        Norte -> Este
        Este  -> Sur
        Sur   -> Oeste
        Oeste -> error "No hay siguiente direccion a Oeste"

--3.2
--a)
data DiaDeSemana = Lunes| Martes| Miercoles | Jueves| Viernes| Sabado | Domingo
    deriving (Show, Eq, Enum, Ord)
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


vienesDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vienesDespues dia1 dia2 = dia1 > dia2

--d)
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio d = d /= primerDia && d /= ultimoDia

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
yTambien bool1 bool2= bool1 && bool2

--d)
oBien :: Bool -> Bool -> Bool
oBien True _= True 
oBien _ True = True
oBien _ _ = False


-- 4 Registros
--4.1
data Persona = P String Int deriving (Show, Eq)
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
laQueEsMayor (P n1 e1) (P n2 e2) = if e1>e2
    then P n1 e1
    else P n2 e2

matias:: Persona
matias = P "Matias"  25
juan :: Persona
juan = P "Juan" 15




