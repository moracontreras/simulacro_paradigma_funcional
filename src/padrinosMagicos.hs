{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Text.Show.Functions ()

--De los chicos se conoce su nombre, edad, sus habilidades y sus deseos.
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

--aprenderHabilidades habilidades unChico : agrega una lista de habilidades
--nuevas a las que ya tiene el chico.
aprenderHabilidades :: [String]->Deseo
aprenderHabilidades habilidades unChico = mapHabilidades unChico (++ habilidades)

{-serGrosoEnNeedForSpeed unChico: dado un chico, le agrega las habilidades
de jugar a todas las versiones pasadas y futuras del Need For Speed, que
son: “jugar need for speed 1”, “jugar need for speed 2”, etc.-}
serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed unChico = mapHabilidades unChico (++jugarVersiones)

jugarVersiones :: [String]
jugarVersiones = map jugar [1 ..]

jugar :: Int->String
jugar unNumero = "jugar" ++ show unNumero

--serMayor unChico: Hace que el chico tenga 18 años.
serMayor :: Deseo
serMayor unChico = mapEdad unChico (const 18)

{-wanda: dado un chico, wanda le cumple el primer deseo y lo hace madurar
(crecer un año de edad).-}
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

{-cosmo: dado un chico, lo hace “des”madurar, quedando con la mitad de años de
edad. Como es olvidadizo, no le concede ningún deseo.-}
cosmo :: Chico->Chico
cosmo unChico = mapEdad unChico (div 2)

timmy :: Chico
timmy= UnChico "Timmy" 10 ["mirar television", "jugar en la pc"] [serMayor]

--muffinMagico: dado un chico le concede todos sus deseos.
muffinMagico :: Chico->Chico
muffinMagico (UnChico nombre edad habilidades []) = UnChico nombre edad habilidades []
muffinMagico (UnChico nombre edad habilidades (cabeza:cola)) = muffinMagico.cumplirDeseo $(UnChico nombre edad habilidades (cabeza:cola))

muffinMagico2 :: Chico->Chico
muffinMagico2 unChico = foldl (\chico deseo-> cumplirDeseo chico) unChico (deseos unChico)

{-tieneHabilidad unaHabilidad unChico: Dado un chico y una habilidad, dice
si la posee.-}
tieneHabilidad::String->Chico->Bool
tieneHabilidad unaHabilidad unChico = elem unaHabilidad.habilidades $unChico

{-esSuperMaduro: Dado un chico dice si es mayor de edad (es decir, tiene más
de 18 años) y además sabe manejar.-}
esSuperMaduro :: Chico->Bool
esSuperMaduro unChico = edad unChico >18 && tieneHabilidad "manejar" unChico

{-Las chicas tienen un nombre, y una condición para elegir al chico con el que van ir al
baile.-}
data Chica = UnaChica{
    nombreChica:: String,
    condicion:: Condicion
}deriving Show

type Condicion = Chico ->Bool
trixie = UnaChica "Trixie Tang" noEsTimmy
vicky = UnaChica "Vicky" (tieneHabilidad "ser un supermodelo noruego")

noEsTimmy:: Chico->Bool
noEsTimmy (UnChico nombre edad habilidades deseos) = nombre /= "timmy"

{-quienConquistaA unaChica losPretendientes: Dada una chica y una lista
de pretendientes, devuelve al que se queda con la chica, es decir, el primero
que cumpla con la condición que ella quiere. Si no hay ninguno que la cumpla,
devuelve el último pretendiente (una chica nunca se queda sola). (Sólo en este
punto se puede usar recursividad)-}
quienConquistaA:: Chica->[Chico]->Chico
quienConquistaA (UnaChica _ condicion) unosPretendientes 
    | (length (filtrarChicos condicion unosPretendientes) )== 0 = devolverUltimo unosPretendientes
    | otherwise = devolverPrimero condicion unosPretendientes

filtrarChicos :: (Chico ->Bool)->[Chico]->[Chico]
filtrarChicos condicion unosChicos = filter condicion unosChicos

devolverPrimero :: (Chico->Bool)->[Chico]->Chico
devolverPrimero condicion unosChicos = head (filtrarChicos condicion unosChicos)

devolverUltimo :: [Chico]->Chico
devolverUltimo unosChicos = last unosChicos

{-infractoresDeDaRules : Dada una lista de
chicos, devuelve la lista de los nombres de
aquellos que tienen deseos prohibidos. Un deseo
está prohibido si, al aplicarlo, entre las
cinco primeras habilidades, hay alguna prohibida.
En tanto, son habilidades prohibidas enamorar,
matar y dominar el mundo.-}
infraccionesDeDaRules :: [Chico]->[Chico]
infraccionesDeDaRules unosChicos = filter noTieneHabilidadesProhibidas unosChicos

noTieneHabilidadesProhibidas :: Chico->Bool
noTieneHabilidadesProhibidas (UnChico _ _ habilidades _) = any esHabilidadProhibida.(take 5) $habilidades

esHabilidadProhibida :: String->Bool
esHabilidadProhibida unaHabilidad = elem unaHabilidad ["enamorar","matar","dominar el mundo"]
