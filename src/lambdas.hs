import Text.Show.Functions ()
import Data.Char (isUpper, isDigit)
import Data.Char (toUpper)

{-Los codiciados tesoros tienen un año de descubrimiento 
(en otras palabras, el año en que se registró su existencia por primera vez) y un precio en pesos.-}
type Tesoro = (Año,Precio)
type Año = Int
type Precio = Int

año :: Tesoro->Int
año (año,_)= año

precio :: Tesoro->Int
precio (_,precio) = precio
{-de qué tipo es cierto tesoro, los tipos que conocemos son:
de lujo: si su precio es mayor a $1000 o si su antigüedad es mayor a 200 años. La antigüedad del tesoro puede medirse como la diferencia entre el año actual y su año de descubrimiento.
tela sucia: si vale menos de $50 y no es de lujo.
estándar: si no es de ningún otro tipo.-}
tipoTesoro :: Tesoro->String
tipoTesoro unTesoro 
    | antigüedad unTesoro >200 && precio unTesoro > 1000 = "de lujo"
    | precio unTesoro < 50 = "tela sucia"
    | otherwise = "estándar"

antigüedad :: Tesoro->Int
antigüedad unTesoro = 2024- año unTesoro   

--el valor de un tesoro; se calcula como su precio + 2 * su antigüedad.
valor :: Tesoro->Int
valor unTesoro = (+ precio unTesoro).(*2).antigüedad $ unTesoro

{-Cada cerradura tiene una clave compuesta de letras, números o cualquier otro tipo de carácter.
Decimos que una cerradura está abierta cuando su clave está vacía, es decir, no posee ningún carácter-}
type Clave = String
type Cerradura = Clave

estaVacia :: Cerradura->Bool
estaVacia cerradura = null cerradura

type Herramienta = Cerradura->Cerradura

--el martillo, que afloja la cerradura haciéndole perder los primeros 3 caracteres a su clave.
martillo :: Herramienta
martillo unaCerradura = eliminarCantidadCaracteres 3 unaCerradura

--la llave maestra, que abre cualquier cerradura, dejando su clave vacía.
llaveMaestra :: Herramienta
llaveMaestra unaCerradura = (const []) unaCerradura

eliminarCantidadCaracteres :: Int->String->String
eliminarCantidadCaracteres unaCantidad unaCadena = drop unaCantidad unaCadena

--las ganzúas, todas ellas eliminan el primer carácter de la clave y además cada 
--tipo de ganzúa elimina ciertos caracteres de la clave:
ganzúa :: Herramienta
ganzúa unaCerradura = eliminarCantidadCaracteres 1 unaCerradura

eliminarCaracteresSegun :: (Char->Bool)->String->String
eliminarCaracteresSegun criterio unaCadena =filter (not.criterio) unaCadena

--la ganzúa gancho elimina todas las letras mayúsculas de la clave de una cerradura.
ganzúaGancho :: Herramienta
ganzúaGancho unaCerradura = eliminarCaracteresSegun isUpper.ganzúa $ unaCerradura

--la ganzúa rastrillo elimina todos los números de la clave de una cerradura.
ganzúaRastrillo :: Herramienta
ganzúaRastrillo unaCerradura = eliminarCaracteresSegun isDigit.ganzúa $ unaCerradura

--la ganzúa rombo, que viene con una inscripción, y elimina de la clave de una cerradura 
--todos los caracteres contenidos en esa inscripción.
ganzúaRombo :: String->Herramienta
ganzúaRombo unaInscripcion unaCerradura = eliminarCaracteresSegun (flip elem unaInscripcion).ganzúa $ unaCerradura

--el tensor, que dada una cerradura, convierte todas las minúsculas de su clave en mayúsculas.
tensor :: Herramienta
tensor unaCerradura = map toUpper unaCerradura

--el socotroco, que es un artilugio compuesto por 2 herramientas, y usarlo es equivalente a usar una de las herramientas y luego la otra.
socotroco :: Herramienta->Herramienta->Herramienta
socotroco unaHerramienta otraHerramienta unaCerradura = otraHerramienta.unaHerramienta $ unaCerradura

{-De un ladrón sabemos su nombre, las herramientas que lleva encima y los tesoros que robó.
Al enfrentarse a un cofre (que tiene una cerradura y un tesoro adentro), el ladrón utiliza en orden las herramientas que lleva-}
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

{-si un ladrón es legendario, lo cual se cumple si tiene más de 100 de experiencia y todos los tesoros que robó son de lujo. 
La experiencia de un ladrón se define como la sumatoria de los valores de sus tesoros.-}
esLegendario :: Ladron->Bool
esLegendario (UnLadron _ _ tesoros) = experiencia tesoros > 100 && (all (=="de lujo") (tiposTesoros tesoros))

tiposTesoros :: [Tesoro]->[String]
tiposTesoros unosTesoros = map tipoTesoro unosTesoros

experiencia :: [Tesoro]->Int
experiencia unosTesoros = sum.map precio $ unosTesoros

{-que un ladrón robe un cofre. Esto hace que consuma sus herramientas en orden hasta que su cerradura quede abierta. Una vez que se abre, el ladrón agrega el tesoro a su lista y se queda con las herramientas que no utilizó. 
Si utilizó todas sus herramientas y no pudo abrir la cerradura, queda sin herramientas y el tesoro no se agrega. Este punto puede ser resuelto utilizando recursividad.-}
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

--opcion mejorada
robarCofre2 ::Cofre->Ladron->Ladron
robarCofre2 unCofre unLadron = consumirHerramientas2 (cerradura unCofre) unLadron

consumirHerramientas2 :: Cerradura->Ladron->Ladron
consumirHerramientas2 unaCerradura (UnLadron nombre [] tesoros) = eliminarHerramientas2 (UnLadron nombre [] tesoros)
consumirHerramientas2 unaCerradura (UnLadron nombre (herramienta:resto) tesoros) 
    | (== []).herramienta $ unaCerradura = (UnLadron nombre resto tesoros)
    | otherwise = flip consumirHerramientas2 (UnLadron nombre resto tesoros).herramienta $ unaCerradura

eliminarHerramientas2 :: Ladron->Ladron
eliminarHerramientas2 unLadron = modificarHerramientas (const []) unLadron

{-un atraco llevado a cabo por un ladrón a un conjunto de cofres. Para llevar a cabo el atraco, 
el ladrón va a ir uno por uno robando todos los cofres.-}
atraco :: [Cofre]->Ladron->Ladron
atraco unosCofres unLadron = foldr robarCofre unLadron unosCofres

{-escribir en un comentario el código para hacer lo siguiente (justificar por qué en caso de que sea imposible):
I. un atraco en el que aparezca una lista infinita en algún lado y que por eso la expresión no termine.

II. un atraco en el que aparezca una lista infinita en algún lado pero que aún así evaluar esa expresión termine.

Nota: la lista infinita puede ser cualquiera involucrada, ya sea la lista de cofres en el atraco, la lista de herramientas 
del ladrón o la clave de la cerradura (que probablemente hayan modelado como un String, que es [Char]).-}
infinitosCofres :: [Cofre]
infinitosCofres = map elCofre [1..]

elCofre :: Int->Cofre
elCofre unNumero = ("a"++ show unNumero,(10,20))

ladronEjemplo ::Ladron
ladronEjemplo = UnLadron "mati" [ganzúaGancho,ganzúaRastrillo] []
{-atraco infinitosCofres ladronEjemplo cuenta con una lista infinita y la 
expresion nunca termina. 
-}

