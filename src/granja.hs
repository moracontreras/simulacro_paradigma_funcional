import Text.Show.Functions ()

data Animal = UnAnimal{
    nombre :: String,
    tipo :: String,
    peso :: Int,
    edad :: Int,
    enfermo :: Bool,
    visitas :: [VisitaMedica]
} deriving Show

type VisitaMedica = (Dias,Costo)
type Dias = Int
type Costo = Int

dias :: VisitaMedica->Dias
dias (dias,_)= dias

costo :: VisitaMedica->Costo
costo (_,costo)= costo

laPasoMal :: Animal->Bool
laPasoMal (UnAnimal _ _ _ _ _ visitas) = any ((>30).costo) visitas

tieneNombreFalopa :: Animal->Bool
tieneNombreFalopa (UnAnimal nombre _ _ _ _ _) = (== 'i').last $ nombre 

modificarPeso :: (Int->Int)->Animal->Animal
modificarPeso unaFuncion unAnimal = unAnimal{peso = unaFuncion.peso $ unAnimal}

type Actividad = Animal->Animal

engordar :: Int->Actividad
engordar unaCantidad unAnimal = modificarPeso (const.hasta5kilosMitadPeso $ unaCantidad) unAnimal

hasta5kilosMitadPeso :: Int->Int
hasta5kilosMitadPeso unaCantidad = min 5 (div unaCantidad 2)

revisar :: Dias->Costo->Actividad
revisar unosDias unCosto unAnimal
    | enfermo unAnimal = engordar 2.agregarleVisita unosDias unCosto $ unAnimal
    | otherwise = unAnimal

modificarVisitas :: ([VisitaMedica]->[VisitaMedica])->Animal->Animal
modificarVisitas unaFuncion unAnimal = unAnimal{visitas = unaFuncion.visitas $ unAnimal}

agregarleVisita :: Dias->Costo->Animal->Animal
agregarleVisita unosDias unCosto unAnimal = modificarVisitas ((unosDias,unCosto):) unAnimal

modificarEdad :: (Int->Int)->Animal->Animal
modificarEdad unaFuncion unAnimal = unAnimal{edad = unaFuncion.edad $ unAnimal}

festejarCumpleaños :: Actividad
festejarCumpleaños unAnimal = modificarPeso (subtract 1).modificarEdad (+1) $ unAnimal

chequearPeso :: Int->Actividad
chequearPeso unPeso unAnimal
    | (>unPeso).peso $ unAnimal = unAnimal
    | otherwise = unAnimal{enfermo = True}    

type Proceso = [Actividad]

proceso :: Proceso->Animal->Animal
proceso unasActividades unAnimal = foldr ($) unAnimal unasActividades

mejora :: Proceso->Animal->Bool
mejora [] _ = False
mejora (actividad1:actividad2:resto) unAnimal
    |esMayorACeroYMenorATres (diferenciaPesos unAnimal. actividad1 $ unAnimal) = mejora (actividad2:resto) unAnimal
    | otherwise = False

diferenciaPesos :: Animal->Animal->Int
diferenciaPesos unAnimal unAnimalModificado = peso unAnimalModificado - peso unAnimal

esMayorACeroYMenorATres :: Int->Bool
esMayorACeroYMenorATres unNumero = unNumero<3 && unNumero>0

tresAnimalesConNombreFalopa :: [Animal]->[Animal]
tresAnimalesConNombreFalopa unosAnimales = take 3.filter tieneNombreFalopa $ unosAnimales

{-Al tener lazy evaluation, primero se evalúa la función para ver cuantos elementos de la lista
necesita para ejecutar y luego se evalúa el parámetro. En este caso, se sabe que tiene que tomar solo
3 valores entonces va a filtrar los animales hasta conseguir 3, sin continuar evaluando la lista infinita
-}

probar ::[Int]->[Int]
probar unosNumeros = take 3.filter (>3) $ unosNumeros