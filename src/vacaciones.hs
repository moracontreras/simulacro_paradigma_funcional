{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Text.Show.Functions ()

{-De cada turista nos interesa:
Sus niveles de cansancio y stress
Si está viajando solo
Los idiomas que habla
-}

data Turista = UnTurista {
    nivelDeCansancio :: Int,
    nivelDeStress :: Int,
    viajaSolo :: Bool,
    idiomas :: [String]
} deriving Show

type Excursion = (Turista ->Turista)
ana :: Turista
ana = UnTurista 0 21 False ["Español"]

beto :: Turista
beto = UnTurista 15 15 True ["Alemán"]

catchi :: Turista
catchi = UnTurista 15 15 True ["Alemán", "Catalán"]

--Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.
irALaPlaya :: Excursion
irALaPlaya unTurista 
    | viajaSolo unTurista = modificarNivelDeCansancio (subtract 5) unTurista 
    | otherwise = modificarNivelDeStress (subtract 1) unTurista

modificarNivelDeCansancio :: (Int->Int)-> Turista->Turista
modificarNivelDeCansancio unaFuncion unTurista = unTurista {nivelDeCansancio = unaFuncion.nivelDeCansancio $unTurista}  

modificarNivelDeStress :: (Int->Int)-> Turista->Turista
modificarNivelDeStress unaFuncion unTurista = unTurista {nivelDeStress = unaFuncion.nivelDeStress $unTurista}

--Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia. 
apreciarAlgunElementoDelPaisaje :: String -> Excursion
apreciarAlgunElementoDelPaisaje unElemento unTurista = modificarNivelDeStress (subtract (length unElemento)) unTurista

{-Salir a hablar un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado-}
salirAHablarUnIdioma :: String -> Excursion
salirAHablarUnIdioma unIdioma unTurista = unTurista {idiomas= unIdioma : idiomas unTurista, viajaSolo= True}

{-Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según la intensidad de la caminad, ambos en la misma cantidad. 
El nivel de intensidad se calcula en 1 unidad cada 4 minutos que se caminen.
-}
caminarCiertosMinutos :: Int -> Excursion
caminarCiertosMinutos cantidadMinutos unTurista = modificarNivelDeStress (subtract (intensidadCaminata cantidadMinutos)). modificarNivelDeCansancio (+(intensidadCaminata cantidadMinutos)) $unTurista

intensidadCaminata :: Int->Int
intensidadCaminata cantidadMinutos = cantidadMinutos*4

{-Paseo en barco: depende de cómo esté la marea
si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
si está moderada, no pasa nada.
si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, y sale a hablar con los tripulantes alemanes
-}
paseoEnBarco :: String -> Excursion
paseoEnBarco estadoMarea unTurista 
    | estadoMarea == "fuerte" = modificarNivelDeCansancio (+10).modificarNivelDeStress(+6) $unTurista
    | estadoMarea == "tranquila" = caminarCiertosMinutos 10.apreciarAlgunElementoDelPaisaje "mar".salirAHablarUnIdioma "Alemán" $unTurista
    | otherwise = unTurista

--Punto 2
{-Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los efectos propios de la excursión, 
reduce en un 10% su stress.
-}
hacerExcursion :: Excursion->Turista->Turista
hacerExcursion unaExcursion unTurista= modificarNivelDeStress (subtract (porcentaje (nivelDeStress unTurista))).unaExcursion $unTurista

porcentaje :: Int -> Int
porcentaje unValor = div (unValor*10) 100

{-porcentajeANumero :: Int -> Int ->Int
porcentajeANumero unPorcentaje unNumero = unNumero * (div unPorcentaje 100)-}

{-Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión determine cuánto varió dicho índice después 
de que el turista haya hecho la excursión. Llamamos índice a cualquier función que devuelva un número a partir de un turista.
-}
deltaExcursionSegun :: (Turista ->Int)->Turista-> Excursion ->Int
deltaExcursionSegun indice unTurista unaExcursion = (indice unTurista) - (indice.unaExcursion $unTurista)

{-Saber si una excursión es educativa para un turista, que implica que termina aprendiendo algún idioma.
-}
esExcursionEducativa :: Excursion->Turista->Bool
esExcursionEducativa unaExcursion unTurista = deltaExcursionSegun (length.idiomas) unTurista unaExcursion >0

--Conocer las excursiones desestresantes para un turista. Estas son aquellas que le reducen al menos 3 unidades de stress al turista.
esExcursionDesestresante :: Turista->Excursion->Bool
esExcursionDesestresante  unTurista unaExcursion= deltaExcursionSegun nivelDeStress unTurista unaExcursion >= 3

excursionesDesestresantes :: Turista->Tour->Tour 
excursionesDesestresantes  unTurista excursiones= filter (esExcursionDesestresante unTurista) excursiones
--map me darias lista de booleanos
--Punto 3
type Tour = [Excursion]

{-Completo: Comienza con una caminata de 20 minutos para apreciar una "cascada", luego se camina 40 minutos hasta una playa, 
y finaliza con una salida con gente local que habla "melmacquiano".
-}
completo :: Tour
completo = [caminarCiertosMinutos 20, apreciarAlgunElementoDelPaisaje "cascada", caminarCiertosMinutos 40, salirAHablarUnIdioma "Melmacquiano"]

{-Lado B: Este tour consiste en ir al otro lado de la isla a hacer alguna excursión (de las existentes) que elija el turista. 
Primero se hace un paseo en barco por aguas tranquilas (cercanas a la costa) hasta la otra punta de la isla, luego realiza 
la excursión elegida y finalmente vuelve caminando hasta la otra punta, tardando 2 horas.
-}
ladoB :: Excursion->Tour
ladoB unaExcursion = [paseoEnBarco "tranquila", unaExcursion, caminarCiertosMinutos 120]

{-Isla Vecina: Se navega hacia una isla vecina para hacer una excursión. Esta excursión depende de cómo esté la marea al llegar a la otra 
isla: si está fuerte se aprecia un "lago", sino se va a una playa. En resumen, este tour implica hacer un paseo en barco hasta la isla vecina, 
luego llevar a cabo dicha excursión, y finalmente volver a hacer un paseo en barco de regreso. La marea es la misma en todo el camino.-}
islaVecina :: String -> Tour
islaVecina estadoMarea = [paseoEnBarco estadoMarea, excursionSegunMarea estadoMarea, paseoEnBarco estadoMarea]

{-Hacer que un turista haga un tour. Esto implica, primero un aumento del stress en tantas unidades como cantidad de excursiones 
tenga el tour, y luego realizar las excursiones en orden.
-}
hacerTour :: Tour->Turista->Turista
hacerTour unTour unTurista = foldl (flip hacerExcursion) (modificarNivelDeStress (+length unTour) unTurista) $unTour
   
--cada vez que haga hacerExcursion por cada excursion, y por cada hacerExcursion modifique el nivel de estres
-- foldl (+) 42 [1,2,3,4] es 42+1 , 42 + 1 + 2 // si fuera foldr es 4 - 3, 4-3-2, 4-3-2-1 
-- 52


excursionSegunMarea :: String -> Excursion
excursionSegunMarea estadoMarea
    | estadoMarea == "fuerte" = apreciarAlgunElementoDelPaisaje "lago"
    | otherwise = irALaPlaya

{-Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. Esto significa que el tour
 tiene alguna excursión desestresante la cual, además, deja al turista acompañado luego de realizarla.-}
algunoEsConveniente :: [Tour]->Turista->Bool
algunoEsConveniente conjuntoTours unTurista = any (esTourConvincente unTurista) conjuntoTours 


esTourConvincente :: Turista->Tour->Bool
esTourConvincente unTurista unTour = any (dejaAcompañado unTurista).(excursionesDesestresantes unTurista) $unTour

dejaAcompañado :: Turista -> Excursion -> Bool
dejaAcompañado unTurista unaExcursion = not.viajaSolo.(hacerExcursion unaExcursion) $unTurista


{-Saber la efectividad de un tour para un conjunto de turistas. 
Esto se calcula como la sumatoria de la espiritualidad recibida 
de cada turista a quienes les resultó convincente el tour. 
La espiritualidad que recibe un turista es la suma de las 
pérdidas de stress y cansancio tras el tour. -}

--espiritualidadRecibida :: [Turista]->Tour->Int
--espiritualidadRecibida unosTuristas unTour =  foldl ((flip hacerTour).(filter (flip esTourConvincente unTour))) nivelDeStress $unosTuristas

