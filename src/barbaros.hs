{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Text.Show.Functions ()
import Data.Char (toUpper)
import Data.Char (isUpper)

{-Se sabe que los bárbaros tienen nombre, fuerza, habilidades y objetos, que los ayudarán más adelante en su lucha contra el mal. 
Por ejemplo: 
dave = Barbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]
-}

data Barbaro= UnBarbaro {
    nombre :: String,
    fuerza :: Int,
    habilidades :: [String],
    objetos :: [Objeto]
} deriving Show

type Objeto = Barbaro -> Barbaro

--accessors por separado para no repetir logica


dave :: Barbaro
dave = UnBarbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]

--Las espadas aumentan la fuerza de los bárbaros en 2 unidades por cada kilogramo de peso.
espadas :: Int->Objeto
espadas unPeso unBarbaro= unBarbaro {fuerza = fuerza unBarbaro + 2* unPeso}

--Los amuletosMisticos puerco-marranos otorgan una habilidad dada a un bárbaro.
amuletosMisticos :: String->Objeto
amuletosMisticos unaHabilidad unBarbaro = unBarbaro {habilidades = unaHabilidad: habilidades unBarbaro}

--Las varitasDefectuosas, añaden la habilidad de hacer magia, pero desaparecen todos los demás objetos del bárbaro.
varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = unBarbaro {habilidades = "hacerMagia":habilidades unBarbaro, objetos= [varitasDefectuosas]}

--Una ardilla, que no hace nada.
ardilla :: Objeto
ardilla unBarbaro = unBarbaro

--Una cuerda, que combina dos objetos distintos,obteniendo uno que realiza las transformaciones de los otros dos.
cuerda :: Objeto->Objeto->Objeto
cuerda objeto1 objeto2 unBarbaro = objeto2.objeto1 $unBarbaro

--El megafono es un objeto que potencia al bárbaro, concatenando sus habilidades y poniéndolas en mayúsculas. 
megafono :: Objeto
megafono unBarbaro = unBarbaro {habilidades =((palabrasEnMayuscula.concatenar) (habilidades unBarbaro))}

palabrasEnMayuscula :: [String]-> [String]
palabrasEnMayuscula lista =map (map toUpper) lista

concatenar :: [String]->[String]
concatenar unasPalabras = [concat unasPalabras]

--objeto megafonoBarbarico, que está formado por una cuerda, una ardilla y un megáfono. 
megafonoBarbarico :: Objeto->Objeto->Objeto
megafonoBarbarico ardilla megafono unBarbaro= unBarbaro{objetos= (cuerda ardilla megafono):objetos unBarbaro}

{-Los bárbaros suelen ir de aventuras por el reino luchando contra las fuerzas del mal, 
pero ahora que tienen nuestra ayuda, quieren que se les diga si un grupo de bárbaros puede sobrevivir a cierta aventura.  
Una aventura se compone de uno o más eventos, por ejemplo:
-}
type Aventura = [Evento]
type Evento = Barbaro->Bool

--invasionDeSuciosDuendes: Un bárbaro sobrevive si sabe “Escribir Poesía Atroz”
invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes unBarbaro = elem "Escribir Poesía Atroz".habilidades $unBarbaro

--cremalleraDelTiempo: Un bárbaro sobrevive si no tiene pulgares. Los bárbaros llamados Faffy y Astro no tienen pulgares, los demás sí.
cremalleraDelTiempo :: Evento
cremalleraDelTiempo unBarbaro = noTienePulgares unBarbaro

noTienePulgares :: Barbaro->Bool
noTienePulgares unBarbaro = elem (nombre unBarbaro) ["Faffy","Astro"]

--ritualDeFechorias: Un bárbaro puede sobrevivir si pasa una o más pruebas como las siguientes: 
ritualDeFechorias :: Evento
ritualDeFechorias unBarbaro = any (\evento ->evento unBarbaro) [saqueo, gritoDeGuerra, caligrafia]

--saqueo: El bárbaro debe tener la habilidad de robar y tener más de 80 de fuerza.
saqueo :: Evento
saqueo unBarbaro = elem "robar" (habilidades unBarbaro) && (>=80) (fuerza unBarbaro)

--gritoDeGuerra: El bárbaro debe tener un poder de grito de guerra igual a la cantidad de letras de sus habilidades. 
--El poder necesario para aprobar es 4 veces la cantidad de objetos del bárbaro.
gritoDeGuerra :: Evento
gritoDeGuerra unBarbaro = poderGritoDeGuerra unBarbaro == (cantidadDeLetras.habilidades $unBarbaro)

poderGritoDeGuerra :: Barbaro ->Int
poderGritoDeGuerra unBarbaro = 4*(length.objetos $unBarbaro)

cantidadDeLetras :: [String]->Int
cantidadDeLetras unaLista = sum.map length $unaLista

--caligrafia: El bárbaro tiene caligrafía perfecta (para el estándar barbárico de la época) si sus habilidades contienen más de 3 vocales y comienzan con mayúscula.
caligrafia :: Evento
caligrafia unBarbaro = all tiene3VocalesYComienzanConMayuscula (habilidades unBarbaro)

tiene3VocalesYComienzanConMayuscula :: String->Bool
tiene3VocalesYComienzanConMayuscula unaPalabra = masDeTresVocales unaPalabra && comienzaConMayuscula unaPalabra

masDeTresVocales :: String->Bool        
masDeTresVocales unaPalabra =  cantidadDeVocales unaPalabra > 3

cantidadDeVocales :: String->Int
cantidadDeVocales unaPalabra = length.(filter esVocal) $unaPalabra

esVocal :: Char->Bool
esVocal unaLetra = elem unaLetra vocales

vocales :: String
vocales = "aeiouAEIOU"

comienzaConMayuscula :: String->Bool
comienzaConMayuscula unaPalabra = isUpper.head $unaPalabra

--Definir la función sobrevivientes que tome una lista de bárbaros y una aventura, y diga cuáles bárbaros la sobreviven (es decir, pasan todas las pruebas)
sobrevivientes :: [Barbaro]->Aventura->[Barbaro]
sobrevivientes unosBarbaros unaAventura = filter (\unBarbaro -> all (\evento -> evento unBarbaro) unaAventura) unosBarbaros

--Los bárbaros se marean cuando tienen varias habilidades iguales. Por todo esto, nos piden desarrollar una función que elimine los elementos repetidos de una lista (sin utilizar nub ni nubBy)
sinRepetidos :: (Eq a)=> [a]->[a]
sinRepetidos [] = []
sinRepetidos (x:[]) =[x]
sinRepetidos (cabeza:cola)
    | elem cabeza cola = cola
    | otherwise = (cabeza: cola)

--Los bárbaros son una raza muy orgullosa, tanto que quieren saber cómo van a ser sus descendientes y asegurarse de que los mismos reciban su legado.
--El descendiente de un bárbaro comparte su nombre, y un asterisco por cada generación. Por ejemplo "Dave*", "Dave**" , "Dave***" , etc. 
--Además, tienen en principio su mismo poder, habilidades sin repetidos, y los objetos de su padre, pero antes de pasar a la siguiente generación,
-- utilizan (aplican sobre sí mismos) los objetos

descendiente :: Barbaro->Barbaro
descendiente unBarbaro = utilizarObjetos unBarbaro{habilidades= sinRepetidos.habilidades $unBarbaro, nombre = nombre unBarbaro ++ "*"}

utilizarObjetos :: Barbaro->Barbaro
utilizarObjetos unBarbaro = foldr ($) unBarbaro (objetos unBarbaro)

descendientes :: Barbaro->[Barbaro]
descendientes unBarbaro = iterate descendiente unBarbaro

--Pregunta: ¿Se podría aplicar sinRepetidos sobre la lista de objetos? ¿Y sobre el nombre de un bárbaro? ¿Por qué?

--no podria aplicar sin repetidos sobre objetos xq los objetos no son comparables entre sí x ser funciones
--si se podria aplicar al nombre xq al ser string sí es comparable