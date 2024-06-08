import Text.Show.Functions ()

data Fremen = UnFremen{
    nombre :: String,
    tolerancia :: Int,
    titulos :: [String],
    reconocimientos :: Int
} deriving (Show,Eq)

stilgar :: Fremen
stilgar = UnFremen "Stilgar" 150 ["Guia"] 3

type Tribu = [Fremen]

modificarReconocimientos :: (Int->Int)->Fremen->Fremen
modificarReconocimientos unaFuncion unFremen = unFremen{reconocimientos = unaFuncion.reconocimientos $ unFremen}

recibirReconocimiento :: Fremen->Fremen
recibirReconocimiento unFremen = promocionarReconocimiento unFremen

promocionarReconocimiento :: Fremen->Fremen
promocionarReconocimiento unFremen = modificarReconocimientos (+1) unFremen

hayCandidato :: Tribu->Bool
hayCandidato unosFremen = any esCandidato unosFremen

esCandidato :: Fremen->Bool
esCandidato (UnFremen _ tolerancia titulos _) = tolerancia > 100 && elem "Domador" titulos

elegido ::Tribu->Fremen
elegido unosFremen = head.ordenarPorReconocimiento.filter esCandidato $ unosFremen

ordenarPorReconocimiento :: Tribu->Tribu
ordenarPorReconocimiento (primero:[]) =[primero]
ordenarPorReconocimiento (primero:segundo:resto) 
    |reconocimientos primero > reconocimientos segundo = primero: ordenarPorReconocimiento (segundo:resto)
    |otherwise = ordenarPorReconocimiento (segundo:primero:resto)

type Gusano = (Longitud,Hidratacion,Descripcion)

type Longitud = Int
type Hidratacion = Int
type Descripcion = String

longitud :: Gusano->Int
longitud (longitud,_,_)= longitud

hidratacion :: Gusano->Int
hidratacion (_,hidratacion,_)= hidratacion

descripcion :: Gusano->String
descripcion (_,_,descripcion)= descripcion

gusano1 :: Gusano
gusano1 = (10,5,"rojo con lunares")

gusano2 :: Gusano
gusano2 = (8,1,"dientes puntiagudos")

reproducirse :: Gusano->Gusano->Gusano
reproducirse unGusano otroGusano = (porcentaje 10 (maximaLongitud unGusano otroGusano), 0, concatenarDescripciones unGusano otroGusano)

porcentaje :: Int->Int->Int
porcentaje unPorcentaje unValor = (unValor*unPorcentaje) `div` 100

maximaLongitud :: Gusano->Gusano->Int
maximaLongitud unGusano otroGusano = max (longitud unGusano) (longitud otroGusano)

concatenarDescripciones :: Gusano->Gusano->String
concatenarDescripciones unGusano otroGusano = (descripcion unGusano) ++" "++ (descripcion otroGusano)

aparearse :: [Gusano]->[Gusano]->[Gusano]
aparearse [] _ = []
aparearse _ [] = []
aparearse (uno1:resto1) (uno2:resto2) = reproducirse uno1 uno2 : aparearse resto1 resto2

type Mision = Fremen->Gusano->Fremen

domarGusanoDeArena :: Mision
domarGusanoDeArena unFremen unGusano
    | domaAlGusano unGusano unFremen = aumentar100Tolerancia.modificarTitulos ("Domador":) $ unFremen
    |otherwise = bajarTolerancia 10 unFremen

bajarTolerancia :: Int->Fremen->Fremen
bajarTolerancia unNumero unFremen = modificarTolerancia (porcentaje (100-unNumero)) unFremen

aumentar100Tolerancia :: Fremen->Fremen
aumentar100Tolerancia unFremen = modificarTolerancia (+100) unFremen

modificarTolerancia :: (Int->Int)->Fremen->Fremen
modificarTolerancia unaFuncion unFremen  = unFremen{tolerancia = unaFuncion.tolerancia $ unFremen}

modificarTitulos :: ([String]->[String])->Fremen->Fremen
modificarTitulos unaFuncion unFremen = unFremen{titulos= unaFuncion.titulos $ unFremen}

domaAlGusano :: Gusano->Fremen->Bool
domaAlGusano unGusano (UnFremen _ tolerancia _ _) = tolerancia > (mitadLongitudGusano unGusano)

mitadLongitudGusano :: Gusano->Int
mitadLongitudGusano unGusano = (`div` 2).longitud $ unGusano

destruirGusanoDeArena :: Mision
destruirGusanoDeArena unFremen unGusano
    | destruyeAlGusano unGusano unFremen = aumentar100Tolerancia.promocionarReconocimiento $ unFremen
    | otherwise = bajarTolerancia 20 unFremen

destruyeAlGusano :: Gusano->Fremen->Bool
destruyeAlGusano unGusano (UnFremen _ tolerancia titulos _) = elem "Domador" titulos && tolerancia < (mitadLongitudGusano unGusano)    

inventada :: Mision
inventada unFremen unGusano
    | loHaceBailar unGusano unFremen = aumentar100Tolerancia.modificarTitulos ("Bailarin":).promocionarReconocimiento $ unFremen
    | otherwise = bajarTolerancia 15 unFremen

loHaceBailar :: Gusano->Fremen->Bool
loHaceBailar unGusano (UnFremen _ tolerancia _ _) = tolerancia > (div (mitadLongitudGusano unGusano) 2) 

realizarMisionColectivamente :: Mision->Gusano->Tribu->Tribu
realizarMisionColectivamente unaMision unGusano unaTribu = map (`unaMision` unGusano) unaTribu

cambiaElElegido :: Tribu->Mision->Gusano->Bool
cambiaElElegido unaTribu unaMision unGusano = (elegido unaTribu) /= (elegido.(realizarMisionColectivamente unaMision unGusano) $ unaTribu)

infinitosFremen :: [Fremen]
infinitosFremen = map fremen [1..]

fremen :: Int->Fremen
fremen unNumero = UnFremen ("fremen " ++ show unNumero) 5 [] 1

{-ghci> hayCandidato infinitosFremen
Interrupted.
ghci> elegido infinitosFremen
Interrupted.
Pasa porque primero evalua la funcion, analiza cuanto debe analizar del parametro y luego analiza el parametro
En este caso necesita el parametro completo, entonces nunca termina de evaluarlo para poder completar la funcion.
Si no necesitara todo el parametro, devolveria un resultado ejemplo head.
En otros casos ejemplo filter, va agregando a medida que evalua entonces evalua el 1 y queda pensando
-}