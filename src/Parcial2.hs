import Text.Show.Functions ()

data Ninja = UnNinja{
    nombre :: String,
    herramientas :: [Herramienta],
    jutsus :: [Jutsu],
    rango :: Int
} deriving Show

type Herramienta = (String,Int)
type Jutsu = Mision->Mision

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

usarHerramienta :: Herramienta->Ninja->Ninja
usarHerramienta unaHerramienta unNinja = modificarHerramientas (sacarHerramienta unaHerramienta) unNinja

sacarHerramienta :: Herramienta->[Herramienta]->[Herramienta]
sacarHerramienta unaHerramienta unasHerramientas = filter (/= unaHerramienta) unasHerramientas

data Mision = UnaMision{
    ninjas :: Int,
    rangoRecomendable :: Int,
    enemigos :: [Ninja],
    herramienta :: Herramienta
} deriving Show

esDesafiante :: [Ninja]->Mision->Bool
esDesafiante unosNinjas (UnaMision _ rangoMision enemigos _) = noCumpleRango rangoMision unosNinjas && length enemigos>=2

noCumpleRango :: Int->[Ninja]->Bool
noCumpleRango rangoMision unosNinjas = any (noCumple rangoMision) $ unosNinjas

esCopada :: Mision->Bool
esCopada (UnaMision _ _ _ herramienta) = elem herramienta [("bomba de humo",3),("shuriken",5),("kunai",14)]

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

fallarMision :: [Ninja]->Mision->[Ninja]
fallarMision unosNinjas (UnaMision _ rangoMision _ _) = castigo.filter (cumple rangoMision) $ unosNinjas

cumple :: Int->Ninja->Bool
cumple rangoMision unNinja = (>=rangoMision).rango $ unNinja

noCumple :: Int->Ninja->Bool
noCumple rangoMision unNinja = not (cumple rangoMision unNinja)

castigo :: [Ninja]->[Ninja]
castigo unosNinjas = map (modificarRango2 (subtract 2)) unosNinjas

cumplirMision :: [Ninja]->Mision->[Ninja]
cumplirMision unosNinjas (UnaMision _ _ _ (nombre,cantidad)) = map (obtenerHerramienta (snd (nombre,cantidad)) (fst (nombre,cantidad))).promocionar $unosNinjas

promocionar :: [Ninja]->[Ninja]
promocionar unosNinjas = map (modificarRango2 (+1)) unosNinjas

modificarCantidadNinjas :: (Int->Int)->Mision->Mision
modificarCantidadNinjas unaFuncion unaMision = unaMision{ninjas= unaFuncion.ninjas $ unaMision}

clonesDeSombra :: Int->Jutsu
clonesDeSombra cantidad unaMision 
    | cantidad >=1 = modificarCantidadNinjas (const cantidad) unaMision
    | otherwise = modificarCantidadNinjas (const 1) unaMision

modificarEnemigos :: ([Ninja]->[Ninja])->Mision->Mision
modificarEnemigos unaFuncion unaMision = unaMision{enemigos= unaFuncion.enemigos $ unaMision}

fuerzaDeUnCentenar :: Jutsu
fuerzaDeUnCentenar unaMision = modificarEnemigos sacarPocoRango unaMision

sacarPocoRango :: [Ninja]->[Ninja]
sacarPocoRango unosNinjas = filter ((<500).rango) unosNinjas

ejecutarMision :: [Ninja]->Jutsu
ejecutarMision unosNinjas unaMision = foldr (\mision->ejecutaMision mision) unaMision unosNinjas

ejecutaMision :: Ninja->Jutsu
ejecutaMision unNinja unaMision = foldr ($) unaMision (jutsus unNinja)

granGuerraNinja :: Mision
granGuerraNinja = UnaMision 100000 100 infinitosZetsus ("abanico de Madara Uchiha",1)

infinitosZetsus :: [Ninja]
infinitosZetsus = map zetsu [1 ..]

zetsu :: Int-> Ninja
zetsu unNumero = UnNinja ("Zetsu" ++ show unNumero) [] [] 600
