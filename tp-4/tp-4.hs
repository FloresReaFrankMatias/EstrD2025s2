{-
            1- PIZZAS

-}



data Pizza       = Prepizza      | Capa Ingrediente Pizza 
    deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show
--Pizzas ejemplos
pizza0 = Prepizza
pizza1 = Capa Salsa Prepizza
pizza2 = Capa Queso (Capa Salsa Prepizza)
pizza3 = Capa (Aceitunas 8)
                           (Capa Queso (Capa Salsa Prepizza))
pizza4 = Capa Jamon (Capa Queso Prepizza)


cantidadDeCapas :: Pizza -> Int

cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p    

ingrs = [Salsa, Queso, Jamon]
armarPizza :: [Ingrediente] -> Pizza
armarPizza  []  = Prepizza 
armarPizza  (i:is)  = (Capa i (armarPizza is)) 
{-
sacarJamon :: Pizza -> Pizza
sacarJamon  Prepizza  = Prepizza
sacarJamon (Capa i p) = if esJamon i
                        then sacarJamon p 
                        else (Capa i (sacarJamon p) )

 esJamon :: Ingrediente -> Bool
 esJamon Jamon = True 
 esJamon _     = False
--}
                        
tieneSoloSalsaYQueso :: Pizza -> Bool 
tieneSoloSalsaYQueso Prepizza   = True
tieneSoloSalsaYQueso (Capa i p) = esSalsaOQueso i  && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Queso = True
esSalsaOQueso Salsa = True
esSalsaOQueso _     = False


duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza    = Prepizza
duplicarAceitunas (Capa i p ) = (Capa (duplicarAceitunasSi i) 
                                       (duplicarAceitunas p) 
                                 )




duplicarAceitunasSi :: Ingrediente -> Ingrediente
duplicarAceitunasSi (Aceitunas n) = (Aceitunas (n*2))
duplicarAceitunasSi ing           = ing

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





