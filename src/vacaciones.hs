{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Text.Show.Functions ()



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

irALaPlaya :: Excursion
irALaPlaya unTurista 
    | viajaSolo unTurista = modificarNivelDeCansancio (subtract 5) unTurista 
    | otherwise = modificarNivelDeStress (subtract 1) unTurista

modificarNivelDeCansancio :: (Int->Int)-> Turista->Turista
modificarNivelDeCansancio unaFuncion unTurista = unTurista {nivelDeCansancio = unaFuncion.nivelDeCansancio $unTurista}  

modificarNivelDeStress :: (Int->Int)-> Turista->Turista
modificarNivelDeStress unaFuncion unTurista = unTurista {nivelDeStress = unaFuncion.nivelDeStress $unTurista}

apreciarAlgunElementoDelPaisaje :: String -> Excursion
apreciarAlgunElementoDelPaisaje unElemento unTurista = modificarNivelDeStress (subtract (length unElemento)) unTurista

salirAHablarUnIdioma :: String -> Excursion
salirAHablarUnIdioma unIdioma unTurista = unTurista {idiomas= unIdioma : idiomas unTurista, viajaSolo= True}

caminarCiertosMinutos :: Int -> Excursion
caminarCiertosMinutos cantidadMinutos unTurista = modificarNivelDeStress (subtract (intensidadCaminata cantidadMinutos)). modificarNivelDeCansancio (+(intensidadCaminata cantidadMinutos)) $unTurista

intensidadCaminata :: Int->Int
intensidadCaminata cantidadMinutos = cantidadMinutos*4

paseoEnBarco :: String -> Excursion
paseoEnBarco estadoMarea unTurista 
    | estadoMarea == "fuerte" = modificarNivelDeCansancio (+10).modificarNivelDeStress(+6) $unTurista
    | estadoMarea == "tranquila" = caminarCiertosMinutos 10.apreciarAlgunElementoDelPaisaje "mar".salirAHablarUnIdioma "Alemán" $unTurista
    | otherwise = unTurista

--Punto 2
hacerExcursion :: Excursion->Turista->Turista
hacerExcursion unaExcursion unTurista= modificarNivelDeStress (subtract (porcentaje (nivelDeStress unTurista))).unaExcursion $unTurista

porcentaje :: Int -> Int
porcentaje unValor = div (unValor*10) 100

{-porcentajeANumero :: Int -> Int ->Int
porcentajeANumero unPorcentaje unNumero = unNumero * (div unPorcentaje 100)-}

deltaExcursionSegun :: (Turista ->Int)->Turista-> Excursion ->Int
deltaExcursionSegun indice unTurista unaExcursion = (indice unTurista) - (indice.unaExcursion $unTurista)

esExcursionEducativa :: Excursion->Turista->Bool
esExcursionEducativa unaExcursion unTurista = deltaExcursionSegun (length.idiomas) unTurista unaExcursion >0

esExcursionDesestresante :: Turista->Excursion->Bool
esExcursionDesestresante  unTurista unaExcursion= deltaExcursionSegun nivelDeStress unTurista unaExcursion >= 3

excursionesDesestresantes :: Turista->Tour->Tour 
excursionesDesestresantes  unTurista excursiones= filter (esExcursionDesestresante unTurista) excursiones
--map me darias lista de booleanos
--Punto 3
type Tour = [Excursion]

completo :: Tour
completo = [caminarCiertosMinutos 20, apreciarAlgunElementoDelPaisaje "cascada", caminarCiertosMinutos 40, salirAHablarUnIdioma "Melmacquiano"]

ladoB :: Excursion->Tour
ladoB unaExcursion = [paseoEnBarco "tranquila", unaExcursion, caminarCiertosMinutos 120]

islaVecina :: String -> Tour
islaVecina estadoMarea = [paseoEnBarco estadoMarea, excursionSegunMarea estadoMarea, paseoEnBarco estadoMarea]

hacerTour :: Tour->Turista->Turista
hacerTour unTour unTurista = foldl (flip hacerExcursion) (modificarNivelDeStress (+length unTour) unTurista) $unTour
   
--cada vez que haga hacerExcursion por cada excursion, y por cada hacerExcursion modifique el nivel de estres
-- foldl (+) 42 [1,2,3,4] es 42+1 , 42 + 1 + 2 // si fuera foldr es 4 - 3, 4-3-2, 4-3-2-1 
-- 52


excursionSegunMarea :: String -> Excursion
excursionSegunMarea estadoMarea
    | estadoMarea == "fuerte" = apreciarAlgunElementoDelPaisaje "lago"
    | otherwise = irALaPlaya

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

