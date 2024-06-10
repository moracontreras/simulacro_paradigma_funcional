{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Text.Show.Functions ()

{-Nuestro programa tendrá participantes que cuentan con nombre, trucos de cocina y un plato que es su especialidad. 
Los platos, a su vez, tienen una dificultad que va de 0 a 10 y un conjunto de componentes que nos indican sus ingredientes con sus respectivos pesos en gramos.
-}
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

--endulzar: dada una cantidad de gramos de azúcar, le agrega ese componente a un plato.  
endulzar :: Int->Truco
endulzar cantidadAzucar unPlato = cambiarIngredientes (("Azucar",cantidadAzucar):) unPlato

--salar: la vieja y confiable… dada una cantidad de gramos de sal y un plato, nos retorna el mismo con esa cantidad de sal para que quede flama.
salar :: Int->Truco
salar cantidadSal unPlato = cambiarIngredientes (("Sal",cantidadSal):) unPlato

--darSabor: dadas una cantidad de sal y una de azúcar sala y endulza un plato.
darSabor :: Int->Int->Truco
darSabor cantidadSal cantidadAzucar unPlato= endulzar cantidadAzucar.salar cantidadSal $ unPlato

--duplicarPorcion: se duplica la cantidad de cada componente de un plato… para más placer.
duplicarPorcion :: Truco
duplicarPorcion unPlato=  cambiarIngredientes (map (cambiarCantidad (2*))) $unPlato

cambiarCantidad :: (Int->Int)->Ingrediente->Ingrediente
cambiarCantidad unaFuncion (nombre, cantidad) = (nombre, unaFuncion cantidad)

--simplificar: hay platos que son realmente un bardo. Es por ello que si un plato tiene más de 5 componentes y una dificultad mayor a 7 
--lo vamos a simplificar, sino lo dejamos igual. Simplificar un plato es dejarlo con 5 de dificultad y quitarle aquellos componentes de los que hayamos 
--agregado menos de 10 gramos
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

--esVegano: si no tiene carne, huevos o alimentos lácteos.
esVegano :: Plato->Bool
--esVegano unPlato = all (notElem ["carne","huevos","alimentos lácteos"]) (map fst.ingredientes $ unPlato)
esVegano unPlato = all (\ingrediente -> notElem (fst ingrediente) ["carne","huevos","alimentos lácteos"]) (ingredientes unPlato)

--esSinTacc: si no tiene harina
esSinTacc :: Plato->Bool
esSinTacc unPlato = all (\ingrediente -> (/= "harina") (fst ingrediente)) (ingredientes unPlato)

--esComplejo: cuando tiene más de 5 componentes y una dificultad mayor a 7.
esComplejo :: Plato->Bool
esComplejo unPlato = ((>7).dificultad $ unPlato) && ((>5).length.ingredientes $ unPlato)

--noAptoHipertension: si tiene más de 2 gramos de sal.
noAptoHipertension :: Plato->Bool
noAptoHipertension unPlato =  (>2).snd.sal $ unPlato

sal :: Plato->Ingrediente
sal (UnPlato _ _ ingredientes) = head.filter ((== "Sal").fst) $ingredientes
--filter me devuelve lista aunque sepa que es uno solo entonces uso head para que haskell entienda

{-En la prueba piloto del programa va a estar participando Pepe Ronccino quien tiene unos trucazos bajo la manga como darle sabor a un plato con 2 gramos de sal 
y 5 de azúcar, simplificarlo y duplicar su porción. Su especialidad es un plato complejo y no apto para personas hipertensas.
 
Modelar a Pepe y su plato. 
-}
pepe :: Participante
pepe = UnParticipante "Pepe Ronccino" [darSabor 2 5, simplificar,duplicarPorcion] (UnPlato "plato pepe" 8 [("Sal",4),("Fideos",80),("Carne",20),("Salsa",3),("Queso",4),("Oregano",1)])

--cocinar: es el momento en el que la magia ocurre y vemos como queda finalmente el plato de un participante luego de aplicar todos sus trucos a su especialidad.
cocinar :: Participante->Plato->Plato
cocinar unParticipante unPlato = foldr (\truco plato->truco plato) unPlato (trucos unParticipante)

--esMejorQue: en esta contienda diremos que un plato es mejor que otro si tiene más dificultad pero la suma de los pesos de sus componentes es menor.
esMejorQue :: Plato->Plato->Bool
esMejorQue unPlato otroPlato = esMasDificil unPlato otroPlato && tieneMenoresPesos unPlato otroPlato

esMasDificil :: Plato->Plato->Bool
esMasDificil (UnPlato _ dificultad1 _) (UnPlato _ dificultad2 _) = dificultad1>dificultad2

tieneMenoresPesos :: Plato->Plato->Bool
tieneMenoresPesos (UnPlato _ _ ingredientes1) (UnPlato _ _ ingredientes2) = (totalPeso ingredientes1) < (totalPeso ingredientes2)

totalPeso :: [Ingrediente]->Int
totalPeso unosIngredientes = sum.map snd $ unosIngredientes

{-participanteEstrella (este punto es el único en el que pueden usar recursividad si así lo desean): ¡se picó y no estamos hablando de la cebolla! 
Dada una lista de participantes, diremos que la estrella es quien luego de que todo el grupo cocine tiene el mejor plato.
-}
participanteEstrella :: [Participante]->Participante
participanteEstrella [] = (UnParticipante "" [] (UnPlato "" 0 [("",0)]))
participanteEstrella (cola:[])=cola
participanteEstrella (participante1:participante2:resto) 
    | esMejorQue (cocinar participante1.especialidad $participante1) (cocinar participante2.especialidad $participante2) = participanteEstrella (participante1:resto)
    | otherwise = participanteEstrella (participante2:resto)

--platinum. Este plato tiene de especial que tiene infinitos componentes misteriosos con cantidades incrementales y dificultad 10
platinum :: Plato
platinum = UnPlato "platinum" 10 infinitosIngredientes

infinitosIngredientes :: [Ingrediente]
infinitosIngredientes = map ingrediente [1 ..]

ingrediente :: Int->Ingrediente
ingrediente unNumero = ("Ingrediente " ++ show unNumero,unNumero)

{-¿Qué sucede si aplicamos cada uno de los trucos modelados en la Parte A al platinum?
¿Cuáles de las preguntas de la Parte A (esVegano, esSinTacc, etc.) se pueden responder sobre el platinum? 
¿Se puede saber si el platinum es mejor que otro plato?
-}

--Como los trucos devuelven platos, entonces quedara infinitamente devolviendo ingredientes de platinum
--Ninguna pregunta de la parte a se le podrian aplicar
--No se puede saber si es mejor que otro plato xq totalPeso se quedara sumando indefinidamente 