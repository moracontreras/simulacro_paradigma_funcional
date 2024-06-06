{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Text.Show.Functions ()
import Control.Monad (ap)

data Chico = UnChico{
    nombre :: String,
    edad :: Int,
    habilidades :: [String],
    deseos :: [Deseo]
} deriving Show

type Deseo = Chico->Chico

mapHabilidades :: Chico->([String]->[String])->Chico
mapHabilidades unChico unaFuncion = unChico{habilidades = unaFuncion.habilidades $unChico}

mapEdad :: Chico->(Int->Int)->Chico
mapEdad unChico unaFuncion = unChico{edad = unaFuncion.edad $ unChico}

mapDeseo :: Chico->([Deseo]->[Deseo])->Chico
mapDeseo unChico unaFuncion = unChico{deseos = unaFuncion.deseos $ unChico}

aprenderHabilidades :: [String]->Deseo
aprenderHabilidades habilidades unChico = mapHabilidades unChico (++ habilidades)

serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed unChico = mapHabilidades unChico (++jugarVersiones)

jugarVersiones :: [String]
jugarVersiones = map jugar [1 ..]

jugar :: Int->String
jugar unNumero = "jugar" ++ show unNumero

serMayor :: Deseo
serMayor unChico = mapEdad unChico (const 18)

wanda :: Chico->Chico
wanda unChico = cumplirDeseo.flip mapEdad (+1) $ unChico

cumplirDeseo :: Chico->Chico
cumplirDeseo unChico = aplicarDeseo unChico.tomarPrimerDeseo $unChico

aplicarDeseo :: Chico->Deseo->Chico
aplicarDeseo unChico unDeseo = sacarPrimerDeseo.unDeseo $unChico

sacarPrimerDeseo :: Chico->Chico
sacarPrimerDeseo unChico = mapDeseo unChico tail

tomarPrimerDeseo :: Chico->Deseo
tomarPrimerDeseo unChico = head.deseos $ unChico

cosmo :: Chico->Chico
cosmo unChico = mapEdad unChico (div 2)

timmy :: Chico
timmy= UnChico "Timmy" 10 ["mirar television", "jugar en la pc"] [serMayor]

muffinMagico :: Chico->Chico
muffinMagico (UnChico nombre edad habilidades []) = UnChico nombre edad habilidades []
muffinMagico (UnChico nombre edad habilidades (cabeza:cola)) = muffinMagico.cumplirDeseo $(UnChico nombre edad habilidades (cabeza:cola))

muffinMagico2 :: Chico->Chico
muffinMagico2 unChico = foldl (\chico deseo-> cumplirDeseo chico) unChico (deseos unChico)

tieneHabilidad::String->Chico->Bool
tieneHabilidad unaHabilidad unChico = elem unaHabilidad.habilidades $unChico

esSuperMaduro :: Chico->Bool
esSuperMaduro unChico = edad unChico >18 && tieneHabilidad "manejar" unChico

data Chica = UnaChica{
    nombreChica:: String,
    condicion:: Condicion
}deriving Show

type Condicion = Chico ->Bool
trixie = UnaChica "Trixie Tang" noEsTimmy
vicky = UnaChica "Vicky" (tieneHabilidad "ser un supermodelo noruego")

noEsTimmy:: Chico->Bool
noEsTimmy (UnChico nombre edad habilidades deseos) = nombre /= "timmy"

quienConquistaA:: Chica->[Chico]->Chico
quienConquistaA (UnaChica _ condicion) unosPretendientes = filtrarChicos condicion $unosPretendientes

--seEncontraronChicos :: [Chico]->Bool
--seEncontraronChicos []= 

filtrarChicos :: (Chico ->Bool)->[Chico]->Chico
filtrarChicos condicion unosChicos = head.filter condicion $unosChicos

infraccionesDeDaRules :: [Chico]->[Chico]
infraccionesDeDaRules unosChicos = filter noTieneHabilidadesProhibidas unosChicos

noTieneHabilidadesProhibidas :: Chico->Bool
noTieneHabilidadesProhibidas (UnChico _ _ habilidades _) = any esHabilidadProhibida.(take 5) $habilidades

esHabilidadProhibida :: String->Bool
esHabilidadProhibida unaHabilidad = elem unaHabilidad ["enamorar","matar","dominar el mundo"]
