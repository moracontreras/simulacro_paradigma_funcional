import Text.Show.Functions()

{-Modelar a los animales: escribir un sinónimo de tipo y definir algunos ejemplos de animales como constantes. 
De un animal se sabe su coeficiente intelectual (un número), su especie (un string) 
y sus capacidades (strings).-}
data Animal = UnAnimal{
    coeficiente :: Int,
    especie :: String,
    habilidades :: [String]
} deriving Show

perro :: Animal
perro = UnAnimal 7 "canino" ["jugar con la pelota", "dar la patita"]

gato :: Animal
gato = UnAnimal 8 "felino" ["trepar", "tener muchas vidas"]

modificarCoeficiente :: (Int->Int)->Animal->Animal
modificarCoeficiente unaFuncion unAnimal = unAnimal{coeficiente= unaFuncion.coeficiente $unAnimal}

modificarHabilidades :: ([String]->[String])->Animal->Animal
modificarHabilidades unaFuncion unAnimal = unAnimal{habilidades = unaFuncion.habilidades $unAnimal}

--inteligenciaSuperior: Incrementa en n unidades su coeficiente intelectual
inteligenciaSuperior :: Int->Animal->Animal
inteligenciaSuperior n unAnimal = modificarCoeficiente (+n) unAnimal

--pinkificar: quitarle todas las habilidades que tenía
pinkificar :: Animal->Animal
pinkificar unAnimal = modificarHabilidades (const []) unAnimal

{-superpoderes:  le da habilidades nuevas	
En caso de ser un elefante: le da la habilidad “no tenerle miedo a los ratones”
En caso de ser un ratón con coeficiente intelectual mayor a 100: le agrega la habilidad de “hablar”. 
Si no, lo deja como está. 
-}
superPoderes :: Animal->Animal
superPoderes unAnimal
    | compararEspecie unAnimal "elefante" = modificarHabilidades ("no tenerle miedo a los ratones" :) unAnimal
    | compararEspecie unAnimal "raton" && coeficiente unAnimal >100 = modificarHabilidades ("hablar":) unAnimal
    | otherwise = unAnimal

compararEspecie :: Animal->String->Bool
compararEspecie unAnimal unaEspecie = especie unAnimal == unaEspecie

--antropomórfico: si tiene la habilidad de hablar y su coeficiente es mayor a 60.
esAntropomorfico :: Animal->Bool
esAntropomorfico unAnimal = tieneHabilidad unAnimal "hablar" && coeficiente unAnimal >60

tieneHabilidad :: Animal->String->Bool
tieneHabilidad unAnimal unaHabilidad = elem unaHabilidad.habilidades $unAnimal

{-noTanCuerdo: si tiene más de dos habilidades de hacer sonidos pinkiescos. Hacer una  función pinkiesco, 
que significa que la habilidad empieza con “hacer ”, y luego va seguido de una palabra "pinkiesca", es decir, 
con 4 letras o menos y al menos una vocal. -}
noTanCuerdo :: Animal->Bool
noTanCuerdo unAnimal = 2 == (length.filter pinkiesco.habilidades $unAnimal)

pinkiesco :: String->Bool
pinkiesco unaHabilidad = take 6 unaHabilidad == "hacer " && esPalabraPinkiesca (drop 6 unaHabilidad)

esPalabraPinkiesca :: String->Bool
esPalabraPinkiesca unaPalabra= length unaPalabra <= 4 && cantidadVocales unaPalabra >=1

cantidadVocales :: String->Int    
cantidadVocales unaPalabra = length.filter (flip elem "aeiou") $ unaPalabra

--Modelar a los experimentos: dar un sinónimo de tipo.

data Experimento = UnExperimento{
    transformaciones :: [Animal->Animal],
    criterioExito :: Animal->Bool
}

{-Desarollar experimentoExitoso: Dado un experimento y un animal, indica si al aplicar 
sucesivamente todas las transformaciones se cumple el criterio de éxito. -}
experimentoExitoso :: Animal->Experimento->Bool
experimentoExitoso unAnimal unExperimento= criterioExito unExperimento.(flip aplicarTransformaciones unExperimento) $unAnimal

aplicarTransformaciones :: Animal->Experimento->Animal
aplicarTransformaciones unAnimal unExperimento = foldr (\transformacion animal-> transformacion animal) unAnimal (transformaciones unExperimento)

raton :: Animal
raton = UnAnimal 17 "raton" ["hablar"]

experimentoPrueba :: Experimento
experimentoPrueba = UnExperimento [pinkificar,inteligenciaSuperior 10,superPoderes] esAntropomorfico
{-Desarrollar los siguientes reportes, que a partir de una lista de animales, una lista de capacidades 
y un experimento (o una serie de transformaciones) permitan obtener:
-}
{-una lista con los coeficientes intelectuales de los animales que entre sus capacidades, luego de 
efectuar el experimento, tengan alguna de las capacidades dadas.
-}
reporte1 :: [Animal]->[String]->Experimento->[Int]
reporte1 unosAnimales unasHabilidades unExperimento = map coeficiente.cumplenHabilidadesPostExperimento unasHabilidades unExperimento $unosAnimales

cumplenHabilidades :: [String]->[Animal]->[Animal]
cumplenHabilidades unasHabilidades unosAnimales = filter (\animal -> all (`elem` habilidades animal) unasHabilidades) unosAnimales

efectuarExperimento :: Experimento->[Animal]->[Animal]
efectuarExperimento unExperimento unosAnimales = map (flip aplicarTransformaciones unExperimento) $unosAnimales

cumplenHabilidadesPostExperimento :: [String]->Experimento->[Animal]->[Animal]
cumplenHabilidadesPostExperimento unasHabilidades unExperimento unosAnimales = cumplenHabilidades unasHabilidades.efectuarExperimento unExperimento $unosAnimales

{-una lista con las especie de los animales que, luego de efectuar el experimento, tengan entre sus capacidades todas las capacidades dadas.-}
reporte2 :: [Animal]->[String]->Experimento->[String]
reporte2 unosAnimales unasHabilidades unExperimento = map especie.cumplenHabilidadesPostExperimento unasHabilidades unExperimento $unosAnimales

{-una lista con la cantidad de capacidades de todos los animales que, luego de efectuar el experimento, no tengan ninguna de las capacidades dadas.-}
reporte3 :: [Animal]->[String]->Experimento->[Int]
reporte3 unosAnimales unasHabilidades unExperimento = map (length.habilidades).cumplenHabilidadesPostExperimento unasHabilidades unExperimento $unosAnimales

jirafa :: Animal
jirafa = UnAnimal{
    coeficiente = 500,
    especie = "jirafa",
    habilidades = ["hacer agua", "hacer pii","hacer mee"]
}
{-Aparece un nuevo animal que tiene infinitas capacidades. 
Dar ejemplos de experimentos que se puedan realizar y que no, si los hubiera. 
Justificar conceptualmente. 
-}
--Si aparece un animal con infinitas capacidades, se pueden aplicar todas las transformaciones
--porque pinkificar las va a cambiar por lista vacia, inteligenciaSuperior cambia el coeficiente 
--y da infinitamente capacidades, idem superPoderes

{- Generar todas las posibles palabras pinkiescas. Pistas:
generateWordsUpTo, que toma una longitud y genera una lista con todas las posibles palabras de hasta la longitud dada.
generateWords que toma una longitud y genera una lista de palabras donde todas tienen exactamente la longitud dada. 
-}
--palabrasPinkiescas :: [String]
--palabrasPinkiescas = generateWordsUpTo 4 ++ generateWords 4