import Text.Show.Functions()

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

inteligenciaSuperior :: Int->Animal->Animal
inteligenciaSuperior n unAnimal = modificarCoeficiente (+n) unAnimal

pinkificar :: Animal->Animal
pinkificar unAnimal = modificarHabilidades (const []) unAnimal

superPoderes :: Animal->Animal
superPoderes unAnimal
    | compararEspecie unAnimal "elefante" = modificarHabilidades ("no tenerle miedo a los ratones" :) unAnimal
    | compararEspecie unAnimal "raton" && coeficiente unAnimal >100 = modificarHabilidades ("hablar":) unAnimal
    | otherwise = unAnimal

compararEspecie :: Animal->String->Bool
compararEspecie unAnimal unaEspecie = especie unAnimal == unaEspecie

esAntropomorfico :: Animal->Bool
esAntropomorfico unAnimal = tieneHabilidad unAnimal "hablar" && coeficiente unAnimal >60

tieneHabilidad :: Animal->String->Bool
tieneHabilidad unAnimal unaHabilidad = elem unaHabilidad.habilidades $unAnimal

noTanCuerdo :: Animal->Bool
noTanCuerdo unAnimal = 2 == (length.filter pinkiesco.habilidades $unAnimal)

pinkiesco :: String->Bool
pinkiesco unaHabilidad = take 6 unaHabilidad == "hacer " && esPalabraPinkiesca (drop 6 unaHabilidad)

esPalabraPinkiesca :: String->Bool
esPalabraPinkiesca unaPalabra= length unaPalabra <= 4 && cantidadVocales unaPalabra >=1

cantidadVocales :: String->Int    
cantidadVocales unaPalabra = length.filter (flip elem "aeiou") $ unaPalabra

data Experimento = UnExperimento{
    transformaciones :: [Animal->Animal],
    criterioExito :: Animal->Bool
}

experimentoExitoso :: Animal->Experimento->Bool
experimentoExitoso unAnimal unExperimento= criterioExito unExperimento.(flip aplicarTransformaciones unExperimento) $unAnimal

aplicarTransformaciones :: Animal->Experimento->Animal
aplicarTransformaciones unAnimal unExperimento = foldr (\transformacion animal-> transformacion animal) unAnimal (transformaciones unExperimento)

raton :: Animal
raton = UnAnimal 17 "raton" ["hablar"]

experimentoPrueba :: Experimento
experimentoPrueba = UnExperimento [pinkificar,inteligenciaSuperior 10,superPoderes] esAntropomorfico

reporte1 :: [Animal]->[String]->Experimento->[Int]
reporte1 unosAnimales unasHabilidades unExperimento = map coeficiente.cumplenHabilidadesPostExperimento unasHabilidades unExperimento $unosAnimales

cumplenHabilidades :: [String]->[Animal]->[Animal]
cumplenHabilidades unasHabilidades unosAnimales = filter (\animal -> all (`elem` habilidades animal) unasHabilidades) unosAnimales

efectuarExperimento :: Experimento->[Animal]->[Animal]
efectuarExperimento unExperimento unosAnimales = map (flip aplicarTransformaciones unExperimento) $unosAnimales

cumplenHabilidadesPostExperimento :: [String]->Experimento->[Animal]->[Animal]
cumplenHabilidadesPostExperimento unasHabilidades unExperimento unosAnimales = cumplenHabilidades unasHabilidades.efectuarExperimento unExperimento $unosAnimales

reporte2 :: [Animal]->[String]->Experimento->[String]
reporte2 unosAnimales unasHabilidades unExperimento = map especie.cumplenHabilidadesPostExperimento unasHabilidades unExperimento $unosAnimales

reporte3 :: [Animal]->[String]->Experimento->[Int]
reporte3 unosAnimales unasHabilidades unExperimento = map (length.habilidades).cumplenHabilidadesPostExperimento unasHabilidades unExperimento $unosAnimales

jirafa :: Animal
jirafa = UnAnimal{
    coeficiente = 500,
    especie = "jirafa",
    habilidades = ["hacer agua", "hacer pii", "hacer mee"]
}

--Si aparece un animal con infinitas capacidades, se pueden aplicar todas las transformaciones
--porque pinkificar las va a cambiar por lista vacia, inteligenciaSuperior cambia el coeficiente 
--y da infinitamente capacidades, idem superPoderes

--palabrasPinkiescas :: [String]
--palabrasPinkiescas = generateWordsUpTo 4 ++ generateWords 4