{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Text.Show.Functions ()
import Data.Char (toUpper)
import Data.Char (isUpper)


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

espadas :: Int->Objeto
espadas unPeso unBarbaro= unBarbaro {fuerza = fuerza unBarbaro + 2* unPeso}

amuletosMisticos :: String->Objeto
amuletosMisticos unaHabilidad unBarbaro = unBarbaro {habilidades = unaHabilidad: habilidades unBarbaro}

varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = unBarbaro {habilidades = "hacerMagia":habilidades unBarbaro, objetos= [varitasDefectuosas]}

ardilla :: Objeto
ardilla unBarbaro = unBarbaro

cuerda :: Objeto->Objeto->Objeto
cuerda objeto1 objeto2 unBarbaro = objeto2.objeto1 $unBarbaro

megafono :: Objeto
megafono unBarbaro = unBarbaro {habilidades =((palabrasEnMayuscula.concatenar) (habilidades unBarbaro))}

palabrasEnMayuscula :: [String]-> [String]
palabrasEnMayuscula lista =map (map toUpper) lista

concatenar :: [String]->[String]
concatenar unasPalabras = [concat unasPalabras]

megafonoBarbarico :: Objeto->Objeto->Objeto
megafonoBarbarico ardilla megafono unBarbaro= unBarbaro{objetos= (cuerda ardilla megafono):objetos unBarbaro}

type Aventura = [Evento]
type Evento = Barbaro->Bool

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes unBarbaro = elem "Escribir Poesía Atroz".habilidades $unBarbaro

cremalleraDelTiempo :: Evento
cremalleraDelTiempo unBarbaro = noTienePulgares unBarbaro

noTienePulgares :: Barbaro->Bool
noTienePulgares unBarbaro = elem (nombre unBarbaro) ["Faffy","Astro"]

ritualDeFechorias :: Evento
ritualDeFechorias unBarbaro = any (\evento ->evento unBarbaro) [saqueo, gritoDeGuerra, caligrafia]

saqueo :: Evento
saqueo unBarbaro = elem "robar" (habilidades unBarbaro) && (>=80) (fuerza unBarbaro)

gritoDeGuerra :: Evento
gritoDeGuerra unBarbaro = poderGritoDeGuerra unBarbaro == (cantidadDeLetras.habilidades $unBarbaro)

poderGritoDeGuerra :: Barbaro ->Int
poderGritoDeGuerra unBarbaro = 4*(length.objetos $unBarbaro)

cantidadDeLetras :: [String]->Int
cantidadDeLetras unaLista = sum.map length $unaLista

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

sobrevivientes :: [Barbaro]->Aventura->[Barbaro]
sobrevivientes unosBarbaros unaAventura = filter (\unBarbaro -> all (\evento -> evento unBarbaro) unaAventura) unosBarbaros

sinRepetidos :: (Eq a)=> [a]->[a]
sinRepetidos [] = []
sinRepetidos (x:[]) =[x]
sinRepetidos (cabeza:cola)
    | elem cabeza cola = cola
    | otherwise = (cabeza: cola)

descendiente :: Barbaro->Barbaro
descendiente unBarbaro = utilizarObjetos unBarbaro{habilidades= sinRepetidos.habilidades $unBarbaro, nombre = nombre unBarbaro ++ "*"}

utilizarObjetos :: Barbaro->Barbaro
utilizarObjetos unBarbaro = foldr ($) unBarbaro (objetos unBarbaro)

descendientes :: Barbaro->[Barbaro]
descendientes unBarbaro = iterate descendiente unBarbaro

--no podria aplicar sin repetidos sobre objetos xq los objetos no son comparables entre sí x ser funciones
--si se podria aplicar al nombre xq al ser string sí es comparable