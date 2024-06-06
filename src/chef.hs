{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Text.Show.Functions ()

data Participante = UnParticipante{
    nombre :: String,
    trucos :: [Truco],
    especialidad :: Plato
} deriving Show

data Plato= UnPlato{
    nombrePlato :: String,
    dificultad :: Int,
    ingredientes :: [Ingrediente]
} deriving Show

type Ingrediente = (String,Int)
type Truco = Plato->Plato

cambiarIngredientes :: ([Ingrediente]->[Ingrediente])->Plato->Plato
cambiarIngredientes unaFuncion unPlato = unPlato{ingredientes = unaFuncion.ingredientes $ unPlato}

cambiarDificultad :: (Int->Int)->Plato->Plato
cambiarDificultad unaFuncion unPlato = unPlato{dificultad = unaFuncion.dificultad $ unPlato}

endulzar :: Int->Truco
endulzar cantidadAzucar unPlato = cambiarIngredientes (("Azucar",cantidadAzucar):) unPlato

salar :: Int->Truco
salar cantidadSal unPlato = cambiarIngredientes (("Sal",cantidadSal):) unPlato

darSabor :: Int->Int->Truco
darSabor cantidadSal cantidadAzucar unPlato= endulzar cantidadAzucar.salar cantidadSal $ unPlato

duplicarPorcion :: Truco
duplicarPorcion unPlato=  cambiarIngredientes (map (cambiarCantidad (2*))) $unPlato

cambiarCantidad :: (Int->Int)->Ingrediente->Ingrediente
cambiarCantidad unaFuncion (nombre, cantidad) = (nombre, unaFuncion cantidad)

simplificar :: Truco
simplificar unPlato
    |esComplejo unPlato = bajarDificultad.sacarIngredientesMenos10 $unPlato
    |otherwise = unPlato

bajarDificultad :: Plato->Plato
bajarDificultad unPlato = cambiarDificultad (const 5) unPlato

cantidadIngrediente :: Ingrediente->Int
cantidadIngrediente (nombre,cantidad) = cantidad

sacarIngredientesMenos10 :: Plato->Plato
sacarIngredientesMenos10 unPlato = cambiarIngredientes (filter ((<10).cantidadIngrediente)) unPlato

esVegano :: Plato->Bool
--esVegano unPlato = all (notElem ["carne","huevos","alimentos lácteos"]) (map fst.ingredientes $ unPlato)
esVegano unPlato = all (\ingrediente -> notElem (fst ingrediente) ["carne","huevos","alimentos lácteos"]) (ingredientes unPlato)

esSinTacc :: Plato->Bool
esSinTacc unPlato = all (\ingrediente -> (/= "harina") (fst ingrediente)) (ingredientes unPlato)

esComplejo :: Plato->Bool
esComplejo unPlato = ((>7).dificultad $ unPlato) && ((>5).length.ingredientes $ unPlato)

noAptoHipertension :: Plato->Bool
noAptoHipertension unPlato =  (>2).snd.sal $ unPlato

sal :: Plato->Ingrediente
sal (UnPlato _ _ ingredientes) = head.filter ((== "Sal").fst) $ingredientes
--filter me devuelve lista aunque sepa que es uno solo entonces uso head para que haskell entienda

pepe :: Participante
pepe = UnParticipante "Pepe Ronccino" [darSabor 2 5, simplificar,duplicarPorcion] (UnPlato "plato pepe" 8 [("Sal",4),("Fideos",80),("Carne",20),("Salsa",3),("Queso",4),("Oregano",1)])

cocinar :: Participante->Plato->Plato
cocinar unParticipante unPlato = foldr (\truco plato->truco plato) unPlato (trucos unParticipante)

esMejorQue :: Plato->Plato->Bool
esMejorQue unPlato otroPlato = esMasDificil unPlato otroPlato && tieneMenoresPesos unPlato otroPlato

esMasDificil :: Plato->Plato->Bool
esMasDificil (UnPlato _ dificultad1 _) (UnPlato _ dificultad2 _) = dificultad1>dificultad2

tieneMenoresPesos :: Plato->Plato->Bool
tieneMenoresPesos (UnPlato _ _ ingredientes1) (UnPlato _ _ ingredientes2) = (totalPeso ingredientes1) < (totalPeso ingredientes2)

totalPeso :: [Ingrediente]->Int
totalPeso unosIngredientes = sum.map snd $ unosIngredientes

participanteEstrella :: [Participante]->Participante
participanteEstrella [] = (UnParticipante "" [] (UnPlato "" 0 [("",0)]))
participanteEstrella (cola:[])=cola
participanteEstrella (participante1:participante2:resto) 
    | esMejorQue (cocinar participante1.especialidad $participante1) (cocinar participante2.especialidad $participante2) = participanteEstrella (participante1:resto)
    | otherwise = participanteEstrella (participante2:resto)

platinum :: Plato
platinum = UnPlato "platinum" 10 infinitosIngredientes

infinitosIngredientes :: [Ingrediente]
infinitosIngredientes = map ingrediente [1 ..]

ingrediente :: Int->Ingrediente
ingrediente unNumero = ("Ingrediente " ++ show unNumero,unNumero)

--Como los trucos devuelven platos, entonces quedara infinitamente devolviendo ingredientes de platinum
--Ninguna pregunta de la parte a se le podrian aplicar
--No se puede saber si es mejor que otro plato xq totalPeso se quedara sumando indefinidamente 