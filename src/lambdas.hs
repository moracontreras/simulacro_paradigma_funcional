import Text.Show.Functions ()
import Data.Char (isUpper, isDigit)
import Data.Char (toUpper)


type Tesoro = (Año,Precio)
type Año = Int
type Precio = Int

año :: Tesoro->Int
año (año,_)= año

precio :: Tesoro->Int
precio (_,precio) = precio

tipoTesoro :: Tesoro->String
tipoTesoro unTesoro 
    | antigüedad unTesoro >200 && precio unTesoro > 1000 = "de lujo"
    | precio unTesoro < 50 = "tela sucia"
    | otherwise = "estándar"

antigüedad :: Tesoro->Int
antigüedad unTesoro = 2024- año unTesoro   

valor :: Tesoro->Int
valor unTesoro = (+ precio unTesoro).(*2).antigüedad $ unTesoro

type Clave = String
type Cerradura = Clave

estaVacia :: Cerradura->Bool
estaVacia cerradura = null cerradura

type Herramienta = Cerradura->Cerradura

martillo :: Herramienta
martillo unaCerradura = eliminarCantidadCaracteres 3 unaCerradura

llaveMaestra :: Herramienta
llaveMaestra unaCerradura = (const []) unaCerradura

eliminarCantidadCaracteres :: Int->String->String
eliminarCantidadCaracteres unaCantidad unaCadena = drop unaCantidad unaCadena

ganzúa :: Herramienta
ganzúa unaCerradura = eliminarCantidadCaracteres 1 unaCerradura

eliminarCaracteresSegun :: (Char->Bool)->String->String
eliminarCaracteresSegun criterio unaCadena =filter (not.criterio) unaCadena

ganzúaGancho :: Herramienta
ganzúaGancho unaCerradura = eliminarCaracteresSegun isUpper.ganzúa $ unaCerradura

ganzúaRastrillo :: Herramienta
ganzúaRastrillo unaCerradura = eliminarCaracteresSegun isDigit.ganzúa $ unaCerradura

ganzúaRombo :: String->Herramienta
ganzúaRombo unaInscripcion unaCerradura = eliminarCaracteresSegun (flip elem unaInscripcion).ganzúa $ unaCerradura

tensor :: Herramienta
tensor unaCerradura = map toUpper unaCerradura

socotroco :: Herramienta->Herramienta->Herramienta
socotroco unaHerramienta otraHerramienta unaCerradura = otraHerramienta.unaHerramienta $ unaCerradura

data Ladron = UnLadron {
    nombre :: String,
    herramientas :: [Herramienta],
    tesoros :: [Tesoro]
} deriving Show

type Cofre = (Cerradura,Tesoro)

cerradura :: Cofre->Cerradura
cerradura unCofre= fst unCofre

tesoro :: Cofre->Tesoro
tesoro unCofre = snd unCofre

esLegendario :: Ladron->Bool
esLegendario (UnLadron _ _ tesoros) = experiencia tesoros > 100 && (all (=="de lujo") (tiposTesoros tesoros))

tiposTesoros :: [Tesoro]->[String]
tiposTesoros unosTesoros = map tipoTesoro unosTesoros

experiencia :: [Tesoro]->Int
experiencia unosTesoros = sum.map precio $ unosTesoros

robarCofre :: Cofre -> Ladron -> Ladron
robarCofre unCofre unLadron = consumirHerramientas (cerradura unCofre) (herramientas unLadron) unLadron unCofre

consumirHerramientas :: Cerradura -> [Herramienta] -> Ladron -> Cofre -> Ladron
consumirHerramientas unaCerradura [] unLadron _ = eliminarHerramientas unLadron
consumirHerramientas unaCerradura (herramienta:resto) unLadron unCofre
    | estaVacia (herramienta unaCerradura) = agregarTesoro unCofre unLadron { herramientas = resto }
    | otherwise = consumirHerramientas (herramienta unaCerradura) resto unLadron unCofre

eliminarHerramientas :: Ladron -> Ladron
eliminarHerramientas unLadron = modificarHerramientas (const []) unLadron

modificarHerramientas :: ([Herramienta]->[Herramienta])->Ladron->Ladron
modificarHerramientas unaFuncion unLadron = unLadron{herramientas = unaFuncion.herramientas $ unLadron}

modificarTesoros :: ([Tesoro]->[Tesoro])->Ladron->Ladron
modificarTesoros unaFuncion unLadron = unLadron{tesoros = unaFuncion.tesoros $ unLadron}

agregarTesoro :: Cofre->Ladron->Ladron
agregarTesoro unCofre unLadron = modificarTesoros ((tesoro unCofre):) unLadron

atraco :: [Cofre]->Ladron->Ladron
atraco unosCofres unLadron = foldr robarCofre unLadron unosCofres

infinitosCofres :: [Cofre]
infinitosCofres = map elCofre [1..]

elCofre :: Int->Cofre
elCofre unNumero = ("a"++ show unNumero,(10,20))

ladronEjemplo ::Ladron
ladronEjemplo = UnLadron "mati" [ganzúaGancho,ganzúaRastrillo] []
{-atraco infinitosCofres ladronEjemplo cuenta con una lista infinita y la 
expresion nunca termina. 
-}

{-robarCofre2 :: Cofre->Ladron->Ladron
robarCofre2 unCofre unLadron 
    | abrirCofre unLadron unCofre == [] = 
    | otherwise = modificarHerramientas (const []) unLadron

agregarTesoro2 :: Cofre->Ladron->Ladron
agregarTesoro2 unCofre unLadron = modificarTesoros (tesoro unCofre :) unLadron

abrirCofre :: Cofre->Ladron->Ladron
abrirCofre (UnLadron _ herramientas _) ([],tesoro) = modificarHerramientas (const resto).agregarTesoro2 tesoro ([],tesoro) $ unLadron
abrirCofre (UnLadron nombre (primera:segunda:resto) tesoros) (cerradura,_) = flip abrirCofre (UnLadron nombre (segunda:resto) tesoros).foldr ($) cerradura (primera:[])-}