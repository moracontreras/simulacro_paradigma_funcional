import Text.Show.Functions ()

{-En este mundo conocemos el nombre, las herramientas,
los jutsus y el rango de cada ninja. El rango es un
número que comienza en 0 y no puede ser negativo.-}
data Ninja = UnNinja{
    nombre :: String,
    herramientas :: [Herramienta],
    jutsus :: [Jutsu],
    rango :: Int
} deriving Show

type Herramienta = (String,Int)
type Jutsu = Mision->Mision
{-obtenerHerramienta: cada ninja debe poder
obtener una cantidad específica de una herramienta en particular teniendo en cuenta
que:
i. si la suma de todas sus herramientas más la cantidad a obtener es menor o
igual a 100, puede hacerlo sin problemas;
ii. en caso contrario, obtiene la cantidad que pueda sin excederse de 100
herramientas.-}
obtenerHerramienta :: Int->String->Ninja->Ninja
obtenerHerramienta cantidad nombre unNinja 
    | totalHerramientas unNinja + cantidad <=100 = agregarHerramienta (nombre,cantidad) unNinja
    | otherwise = agregarHerramienta (nombre,100-totalHerramientas unNinja) unNinja

totalHerramientas :: Ninja->Int
totalHerramientas unNinja = sum.map snd.herramientas $ unNinja

modificarHerramientas :: ([Herramienta]->[Herramienta])->Ninja->Ninja
modificarHerramientas unaFuncion unNinja = unNinja{herramientas= unaFuncion.herramientas $ unNinja}

agregarHerramienta :: Herramienta->Ninja->Ninja
agregarHerramienta unaHerramienta unNinja = modificarHerramientas (unaHerramienta :) unNinja

{-usarHerramienta: cuando un ninja usa alguna de sus herramientas no mide cuántas
utiliza, por lo que se queda sin ella y no debe figurar más entre sus pertenencias.-}
usarHerramienta :: Herramienta->Ninja->Ninja
usarHerramienta unaHerramienta unNinja = modificarHerramientas (sacarHerramienta unaHerramienta) unNinja

sacarHerramienta :: Herramienta->[Herramienta]->[Herramienta]
sacarHerramienta unaHerramienta unasHerramientas = filter (/= unaHerramienta) unasHerramientas

{-De cada misión se especifica qué cantidad de ninjas
requiere, el rango recomendable para realizarla, qué ninjas enemigos hay que derrotar y la
herramienta (¡obviamente con su cantidad!) de recompensa en caso de cumplirla con éxito.-}
data Mision = UnaMision{
    ninjas :: Int,
    rangoRecomendable :: Int,
    enemigos :: [Ninja],
    herramienta :: Herramienta
} deriving Show

{-esDesafiante: dado un equipo de ninjas, una misión es desafiante cuando al menos
alguien del equipo tiene menor rango que el recomendado y hay que derrotar al
menos 2 enemigos.-}
esDesafiante :: [Ninja]->Mision->Bool
esDesafiante unosNinjas (UnaMision _ rangoMision enemigos _) = noCumpleRango rangoMision unosNinjas && length enemigos>=2

noCumpleRango :: Int->[Ninja]->Bool
noCumpleRango rangoMision unosNinjas = any (noCumple rangoMision) $ unosNinjas

{-esCopada: esto pasa cuando la recompensa de la misión son 3 bombas de humo, 5
shurikens o 14 kunais.-}
esCopada :: Mision->Bool
esCopada (UnaMision _ _ _ herramienta) = elem herramienta [("bomba de humo",3),("shuriken",5),("kunai",14)]

{-esFactible: para que una misión sea factible no tiene que ser desafiante y además
el grupo debe contar con la cantidad de ninjas necesaria o la suma total de
herramientas del equipo debe ser superior a 500.-}
esFactible :: [Ninja]->Mision->Bool
esFactible unosNinjas unaMision = not (esDesafiante unosNinjas unaMision) && alcanzanNinjasOMuchasHerramientas unosNinjas unaMision

alcanzanNinjasOMuchasHerramientas :: [Ninja]->Mision->Bool
alcanzanNinjasOMuchasHerramientas unosNinjas (UnaMision cantidad _ _ _) = length unosNinjas>=cantidad || (sum.map totalHerramientas $ unosNinjas)>500

modificarRango :: (Int->Int)->Ninja->Ninja
modificarRango unaFuncion unNinja = unNinja{rango= unaFuncion.rango $ unNinja}

modificarRango2 :: (Int->Int)->Ninja->Ninja
modificarRango2 unaFuncion unNinja
    | (unaFuncion.rango $ unNinja) >=0 = unNinja{rango= unaFuncion.rango $ unNinja}
    | otherwise = unNinja{rango= (const 0).rango $ unNinja}

{-fallarMision: la vida no siempre es fácil... ni en nuestro mundo ni en el mundo ninja.
Cuando una misión falla sólo quedan en el equipo quienes tengan el rango
recomendado o superior. Quienes queden sufrirán la vergüenza de ver su rango
disminuido en 2 unidades. ¡Por el resto del equipo no te preocupes! Te prometemos
que están bien.-}
fallarMision :: [Ninja]->Mision->[Ninja]
fallarMision unosNinjas (UnaMision _ rangoMision _ _) = castigo.filter (cumple rangoMision) $ unosNinjas

cumple :: Int->Ninja->Bool
cumple rangoMision unNinja = (>=rangoMision).rango $ unNinja

noCumple :: Int->Ninja->Bool
noCumple rangoMision unNinja = not (cumple rangoMision unNinja)

castigo :: [Ninja]->[Ninja]
castigo unosNinjas = map (modificarRango2 (subtract 2)) unosNinjas

{-cumplirMision: si todo sale bien, se promociona de rango a cada miembro del
equipo. Además obtendrán la recompensa teniendo en cuenta la restricción del
máximo de herramientas.-}
cumplirMision :: [Ninja]->Mision->[Ninja]
cumplirMision unosNinjas (UnaMision _ _ _ (nombre,cantidad)) = map (obtenerHerramienta (snd (nombre,cantidad)) (fst (nombre,cantidad))).promocionar $unosNinjas

promocionar :: [Ninja]->[Ninja]
promocionar unosNinjas = map (modificarRango2 (+1)) unosNinjas

modificarCantidadNinjas :: (Int->Int)->Mision->Mision
modificarCantidadNinjas unaFuncion unaMision = unaMision{ninjas= unaFuncion.ninjas $ unaMision}

{-clonesDeSombra: reduce la cantidad de ninjas que se necesitan para una misión
en el mismo número que los clones de sombra creados. ¡El tamaño del equipo no
puede ser menor a 1!-}
clonesDeSombra :: Int->Jutsu
clonesDeSombra cantidad unaMision 
    | cantidad >=1 = modificarCantidadNinjas (const cantidad) unaMision
    | otherwise = modificarCantidadNinjas (const 1) unaMision

modificarEnemigos :: ([Ninja]->[Ninja])->Mision->Mision
modificarEnemigos unaFuncion unaMision = unaMision{enemigos= unaFuncion.enemigos $ unaMision}

--fuerzaDeUnCentenar: elimina a todos los enemigos con rango menor a 500.
fuerzaDeUnCentenar :: Jutsu
fuerzaDeUnCentenar unaMision = modificarEnemigos sacarPocoRango unaMision

sacarPocoRango :: [Ninja]->[Ninja]
sacarPocoRango unosNinjas = filter ((<500).rango) unosNinjas

{-ejecutarMision. Cuando se ejecuta una misión, todos
los ninjas del grupo usan todos sus jutsus en la misión. Luego, si la misión es copada o
factible, se cumple. Caso contrario la misión se falla.-}
ejecutarMision :: [Ninja]->Jutsu
ejecutarMision unosNinjas unaMision = foldr (\mision->ejecutaMision mision) unaMision unosNinjas

ejecutaMision :: Ninja->Jutsu
ejecutaMision unNinja unaMision = foldr ($) unaMision (jutsus unNinja)

{-Existe la Gran Guerra Ninja, una misión de rango 100
que necesita al menos 100000 ninjas para
completarse, tiene infinitos enemigos y su
recompensa es el abanico de Madara Uchiha.
Se pide modelar la granGuerraNinja sabiendo,
además, que tiene infinitos villanos y son Zetsu 1,
Zetsu 2, Zetsu 3... Zetsu N, el rango de todos es de
600 y no tienen jutsus ni herramientas.
Sabiendo esto y teniendo en cuenta un equipo de
ninjas finitos, responder qué devuelve y por qué en las
siguientes funciones:
a. esDesafiante
b. esCopada
c. fuerzaDeUnCentenar-}
granGuerraNinja :: Mision
granGuerraNinja = UnaMision 100000 100 infinitosZetsus ("abanico de Madara Uchiha",1)

infinitosZetsus :: [Ninja]
infinitosZetsus = map zetsu [1 ..]

zetsu :: Int-> Ninja
zetsu unNumero = UnNinja ("Zetsu" ++ show unNumero) [] [] 600

{-ghci> esDesafiante infinitosZetsus granGuerraNinja
Interrupted.
ghci> esCopada granGuerraNinja
False
ghci> fuerzaDeUnCentenar granGuerraNinja
UnaMision {cantidadNinjas = 100000, rangoRecomendable = 100, enemigos = Interrupted.-}