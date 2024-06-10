import Text.Show.Functions ()

{-En una granja viven animales, de los cuales registramos su nombre, el tipo de animal, el peso, la edad y sabemos si está enfermo, 
lo cual podrá requerir una visita médica de alguna persona veterinaria, que diagnostica los días de recuperación y le cobra un costo por la atención.-}
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

--Queremos saber si un animal la pasó mal, esto implica que alguna de las visitas médicas que le hicieron le implicó más de 30 días de recuperación.
laPasoMal :: Animal->Bool
laPasoMal (UnAnimal _ _ _ _ _ visitas) = any ((>30).costo) visitas

{-Queremos saber si un animal tiene un nombre falopa, esto pasa si la última letra termina en 'i'. Por ejemplo, "gachi" o "pachi" 
además de ser de sagitario, tienen un nombre falopa. "Dorothy" no tiene un nombre falopa.
Sin funciones auxiliares. Solo composicion y aplicacion parcial-}
tieneNombreFalopa :: Animal->Bool
tieneNombreFalopa (UnAnimal nombre _ _ _ _ _) = (== 'i').last $ nombre 

modificarPeso :: (Int->Int)->Animal->Animal
modificarPeso unaFuncion unAnimal = unAnimal{peso = unaFuncion.peso $ unAnimal}

type Actividad = Animal->Animal

{-Engorde
Le dan de comer al animal "x" kilos de alimento balanceado, con lo cual incrementan la mitad de su peso hasta un máximo de 5 kilos. 
Por ejemplo: si a la vaca Dorothy que pesa 690 kilos le damos de comer 12 kilos de alimento balanceado, pasará a pesar 695 kilos. 
Si le damos 4 kilos, pesará 692
-}
engordar :: Int->Actividad
engordar unaCantidad unAnimal = modificarPeso (const.hasta5kilosMitadPeso $ unaCantidad) unAnimal

hasta5kilosMitadPeso :: Int->Int
hasta5kilosMitadPeso unaCantidad = min 5 (div unaCantidad 2)

{-Revisación
Si el animal está enfermo, se le registra una visita médica anotando los días de recuperación y el costo. Además, al darle vitaminas,
 eso equivaldría a que el animal coma 2 kilos de alimento balanceado. Consejo: evite repetir ideas.-}
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

{-Festejo cumple
Le agrega un año más y también se le hace una fiestita, por la emoción el animal pierde un kilo.
-}
festejarCumpleaños :: Actividad
festejarCumpleaños unAnimal = modificarPeso (subtract 1).modificarEdad (+1) $ unAnimal

{-Chequeo de peso
Queremos registrar si un animal está bien de peso, para lo cual tiene que estar por arriba de un peso "x", 
en caso contrario el animal debe quedar enfermo.
-}
chequearPeso :: Int->Actividad
chequearPeso unPeso unAnimal
    | (>unPeso).peso $ unAnimal = unAnimal
    | otherwise = unAnimal{enfermo = True}    

type Proceso = [Actividad]

{-Queremos modelar un proceso, que realiza una serie de actividades sobre un animal. Se pide que además muestre un 
ejemplo de cómo podría evaluar por consola el proceso para cada una de las actividades resueltas en el punto anterior.
In funciones auxiliares, solo composicion y aplicacion parcial
-}
proceso :: Proceso->Animal->Animal
proceso unasActividades unAnimal = foldr ($) unAnimal unasActividades

{-Dado un proceso (lista de actividades) y un animal, queremos saber si el animal mejora sustentablemente el peso, 
esto implica que el peso nunca debe bajar de una actividad a otra y tampoco debe subir más de 3 kilos de una actividad. 
Usar recursividad-}
mejora :: Proceso->Animal->Bool
mejora [] _ = False
mejora (actividad1:actividad2:resto) unAnimal
    |esMayorACeroYMenorATres (diferenciaPesos unAnimal. actividad1 $ unAnimal) = mejora (actividad2:resto) unAnimal
    | otherwise = False

diferenciaPesos :: Animal->Animal->Int
diferenciaPesos unAnimal unAnimalModificado = peso unAnimalModificado - peso unAnimal

esMayorACeroYMenorATres :: Int->Bool
esMayorACeroYMenorATres unNumero = unNumero<3 && unNumero>0

{-Queremos obtener los primeros tres animales que tengan un nombre falopa. Resolverlo solo con funciones de orden superior.-}
tresAnimalesConNombreFalopa :: [Animal]->[Animal]
tresAnimalesConNombreFalopa unosAnimales = take 3.filter tieneNombreFalopa $ unosAnimales

{-Si le pasáramos una cantidad infinita de animales, sería posible obtener un valor computable para la función del punto anterior? 
Justifique su respuesta relacionándolo con un concepto visto en la materia.
-}

{-Al tener lazy evaluation, primero se evalúa la función para ver cuantos elementos de la lista
necesita para ejecutar y luego se evalúa el parámetro. En este caso, se sabe que tiene que tomar solo
3 valores entonces va a filtrar los animales hasta conseguir 3, sin continuar evaluando la lista infinita
-}

probar ::[Int]->[Int]
probar unosNumeros = take 3.filter (>3) $ unosNumeros