import Text.Show.Functions ()

{-La tribu está compuesta por varios Fremen. Cada Fremen tiene un nombre, un nivel de tolerancia a la Especia, 
una serie de títulos y una cantidad de reconocimientos.  
-}
data Fremen = UnFremen{
    nombre :: String,
    tolerancia :: Int,
    titulos :: [String],
    reconocimientos :: Int
} deriving (Show,Eq)

stilgar :: Fremen
stilgar = UnFremen "Stilgar" 150 ["Guia"] 3

type Tribu = [Fremen]

--Averiguar cómo queda un Fremen al recibir un nuevo reconocimiento.
modificarReconocimientos :: (Int->Int)->Fremen->Fremen
modificarReconocimientos unaFuncion unFremen = unFremen{reconocimientos = unaFuncion.reconocimientos $ unFremen}

recibirReconocimiento :: Fremen->Fremen
recibirReconocimiento unFremen = promocionarReconocimiento unFremen

promocionarReconocimiento :: Fremen->Fremen
promocionarReconocimiento unFremen = modificarReconocimientos (+1) unFremen

--Saber si hay algún candidato a ser el elegido. Son candidatos quienes tengan el título de "Domador" y una tolerancia a la especia de más de 100.
hayCandidato :: Tribu->Bool
hayCandidato unosFremen = any esCandidato unosFremen

esCandidato :: Fremen->Bool
esCandidato (UnFremen _ tolerancia titulos _) = tolerancia > 100 && elem "Domador" titulos

--Hallar al Elegido: Es el Fremen de la tribu que más reconocimientos tenga entre los candidatos a serlo. 
elegido ::Tribu->Fremen
elegido unosFremen = head.ordenarPorReconocimiento.filter esCandidato $ unosFremen

ordenarPorReconocimiento :: Tribu->Tribu
ordenarPorReconocimiento (primero:[]) =[primero]
ordenarPorReconocimiento (primero:segundo:resto) 
    |reconocimientos primero > reconocimientos segundo = primero: ordenarPorReconocimiento (segundo:resto)
    |otherwise = ordenarPorReconocimiento (segundo:primero:resto)

{-Se conoce su longitud, su nivel de hidratación y una descripción. 
Los gusanos se reproducen. Dados dos gusanos, la cría nace de la siguiente manera:
Su longitud es el 10% de la máxima longitud de sus dos progenitores
Su nivel de hidratación es 0
La descripción es la concatenación de ambas descripciones de sus progenitores. 
-}

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

{-Obtener la lista de crías que surge de aparear dos listas de gusanos, uno a uno. En caso que un gusano no tenga con qué otro gusano aparearse, 
obviamente no hay cría. Por ejemplo, el primero de la primera lista con el primero de la segunda lista, los segundos entre sí y así sucesivamente.
-}
aparearse :: [Gusano]->[Gusano]->[Gusano]
aparearse [] _ = []
aparearse _ [] = []
aparearse (uno1:resto1) (uno2:resto2) = reproducirse uno1 uno2 : aparearse resto1 resto2

type Mision = Fremen->Gusano->Fremen

{-Domar gusano de arena: Un Fremen puede domar a un gusano de arena si su nivel de tolerancia a la Especia es al menos la mitad de la longitud del gusano. 
Al hacerlo, obtiene el título de "Domador" y su tolerancia a la especia aumenta en 100 unidades. Si no lo puede hacer su tolerancia a la Especia baja un 10%. 
-}
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

{-Destruir gusano de arena: Un Fremen puede destruir a un gusano de arena si tiene el título de "Domador" y si su nivel de tolerancia 
a la Especia es menor que la mitad de la longitud del gusano. Al hacerlo, recibe un reconocimiento y su tolerancia a la especia aumenta en 100 unidades. 
Si no lo logra, su especia baja un 20%-}
destruirGusanoDeArena :: Mision
destruirGusanoDeArena unFremen unGusano
    | destruyeAlGusano unGusano unFremen = aumentar100Tolerancia.promocionarReconocimiento $ unFremen
    | otherwise = bajarTolerancia 20 unFremen

destruyeAlGusano :: Gusano->Fremen->Bool
destruyeAlGusano unGusano (UnFremen _ tolerancia titulos _) = elem "Domador" titulos && tolerancia < (mitadLongitudGusano unGusano)    

{-Inventada: Inventar otra misión que un Fremen pueda hacer con un gusano, que también se pueda realizar dependiendo de cómo sea el gusano en relación al Fremen 
y que provoque consecuencias diferentes sobre el Fremen si lo logra o no.
-}
inventada :: Mision
inventada unFremen unGusano
    | loHaceBailar unGusano unFremen = aumentar100Tolerancia.modificarTitulos ("Bailarin":).promocionarReconocimiento $ unFremen
    | otherwise = bajarTolerancia 15 unFremen

loHaceBailar :: Gusano->Fremen->Bool
loHaceBailar unGusano (UnFremen _ tolerancia _ _) = tolerancia > (div (mitadLongitudGusano unGusano) 2) 

{-Simular la realización colectiva de una misión: Dada una tribu, una misión cualquiera y un gusano de arena, hacer que cada uno de los Fremen de la tribu intenten 
llevar a cabo la misión con dicho gusano, obteniendo cómo queda la tribu en consecuencia.-}
realizarMisionColectivamente :: Mision->Gusano->Tribu->Tribu
realizarMisionColectivamente unaMision unGusano unaTribu = map (`unaMision` unGusano) unaTribu

{-Averiguar, para una tribu, una misión y un gusano, si el hecho de realizarla colectivamente haría que el elegido de la tribu fuera un Fremen diferente al que hubieran elegido previamente-}
cambiaElElegido :: Tribu->Mision->Gusano->Bool
cambiaElElegido unaTribu unaMision unGusano = (elegido unaTribu) /= (elegido.(realizarMisionColectivamente unaMision unGusano) $ unaTribu)

{-Qué pasaría con una tribu de infinitos Fremen?
Al entrenarlos
Al querer saber si hay algún candidato a ser elegido
Al encontrar al elegido
-}
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