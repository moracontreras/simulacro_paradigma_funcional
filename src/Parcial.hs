module Parcial where
import Text.Show.Functions()

doble :: Int -> Int
doble = (*2)
{-En este mundo conocemos el nombre, las herramientas,
los jutsus y el rango de cada ninja. El rango es un
número que comienza en 0 y no puede ser negativo.-}
data Ninja = UnNinja{
    nombre :: String,
    herramientas :: [Herramienta],
    jutsu :: [Jutsu],
    rango :: Int
} deriving Show

data Herramienta = UnaHerramienta {
    nombreHerramienta :: String,
    cantidadDisponible :: Int
} deriving (Eq,Show)

type Jutsu = Mision->Mision

--accessors
accessorHerramientas :: Ninja->([Herramienta]->[Herramienta])->Ninja
accessorHerramientas unNinja unaFuncion = unNinja{herramientas= unaFuncion.herramientas $unNinja}
{-obtenerHerramienta: cada ninja debe poder
obtener una cantidad específica de una herramienta en particular teniendo en cuenta
que:
i. si la suma de todas sus herramientas más la cantidad a obtener es menor o
igual a 100, puede hacerlo sin problemas;
ii. en caso contrario, obtiene la cantidad que pueda sin excederse de 100
herramientas.-}
obtenerHerramienta :: Int->String->Ninja->Ninja
obtenerHerramienta unaCantidad unNombreHerramienta unNinja
    | unaCantidad + cantidadTotalHerramientas unNinja <= 100 = accessorHerramientas unNinja (armarHerramienta unaCantidad unNombreHerramienta:)
    | otherwise = accessorHerramientas unNinja ((armarHerramienta (tomarHasta100 unaCantidad (cantidadTotalHerramientas unNinja)) unNombreHerramienta):)

tomarHasta100 :: Int->Int->Int
tomarHasta100 unaCantidad unTotal
    |(unaCantidad+unTotal)>100 = 100-unTotal
    |otherwise= unaCantidad+unTotal

armarHerramienta :: Int->String->Herramienta
armarHerramienta unaCantidad unNombre = UnaHerramienta{nombreHerramienta= unNombre, cantidadDisponible = unaCantidad}
cantidadTotalHerramientas :: Ninja->Int
cantidadTotalHerramientas unNinja= sum.map cantidadDisponible.herramientas $unNinja

{-usarHerramienta: cuando un ninja usa alguna de sus herramientas no mide cuántas
utiliza, por lo que se queda sin ella y no debe figurar más entre sus pertenencias.-}
usarHerramienta :: Ninja->Herramienta->Ninja
usarHerramienta unNinja unaHerramienta = (eliminarHerramienta unNinja).nombreHerramienta $unaHerramienta

eliminarHerramienta :: Ninja->String->Ninja
eliminarHerramienta unNinja nombreHerramienta = accessorHerramientas unNinja (sacarHerramienta nombreHerramienta)

sacarHerramienta :: String->[Herramienta]->[Herramienta]
sacarHerramienta unNombre unasHerramientas = filter ((/=unNombre).nombreHerramienta) unasHerramientas

{-De cada misión se especifica qué cantidad de ninjas
requiere, el rango recomendable para realizarla, qué ninjas enemigos hay que derrotar y la
herramienta (¡obviamente con su cantidad!) de recompensa en caso de cumplirla con éxito.-}
data Mision = UnaMision{
    cantidadNinjas :: Int,
    rangoRecomendable :: Int,
    enemigos :: [Ninja],
    recompensa :: Herramienta
} deriving Show

{-esDesafiante: dado un equipo de ninjas, una misión es desafiante cuando al menos
alguien del equipo tiene menor rango que el recomendado y hay que derrotar al
menos 2 enemigos.-}
esDesafiante :: [Ninja]->Mision->Bool
esDesafiante unosNinjas unaMision = any ((rangoRecomendable unaMision>).rango) $unosNinjas

{-esCopada: esto pasa cuando la recompensa de la misión son 3 bombas de humo, 5
shurikens o 14 kunais.-}
esCopada :: Mision->Bool
esCopada unaMision = any (== recompensa unaMision) [armarHerramienta 3 "bomba de humo", armarHerramienta 5 "shurikens", armarHerramienta 14 "kunais"] 

{-esFactible: para que una misión sea factible no tiene que ser desafiante y además
el grupo debe contar con la cantidad de ninjas necesaria o la suma total de
herramientas del equipo debe ser superior a 500.-}
esFactible :: [Ninja]->Mision->Bool
esFactible unosNinjas unaMision = not (esDesafiante unosNinjas unaMision) && cumpleFactibilidad unosNinjas unaMision

cumpleFactibilidad :: [Ninja]->Mision->Bool
cumpleFactibilidad unosNinjas (UnaMision cantidadEquipo _ _ _) =length unosNinjas >= cantidadEquipo || (sum.map cantidadTotalHerramientas $unosNinjas) >500 

{-fallarMision: la vida no siempre es fácil... ni en nuestro mundo ni en el mundo ninja.
Cuando una misión falla sólo quedan en el equipo quienes tengan el rango
recomendado o superior. Quienes queden sufrirán la vergüenza de ver su rango
disminuido en 2 unidades. ¡Por el resto del equipo no te preocupes! Te prometemos
que están bien.-}
fallarMision :: [Ninja]->Mision->[Ninja]
fallarMision unosNinjas (UnaMision _ rangoSugerido _ _) = (map (disminuirRango 2)).filter (cumpleRango rangoSugerido) $ unosNinjas

cumpleRango :: Int->Ninja->Bool
cumpleRango unRango unNinja= unRango <= rango unNinja

disminuirRango :: Int -> Ninja -> Ninja
disminuirRango unaCantidad unNinja = accessorRango (subtract unaCantidad) unNinja

accessorRango :: (Int->Int)->Ninja->Ninja
accessorRango unaFuncion unNinja = unNinja{rango = unaFuncion.rango $unNinja}

{-cumplirMision: si todo sale bien, se promociona de rango a cada miembro del
equipo. Además obtendrán la recompensa teniendo en cuenta la restricción del
máximo de herramientas.-}
cumplirMision :: [Ninja]->Mision->[Ninja]
cumplirMision unosNinjas (UnaMision _ _ _ (UnaHerramienta nombre cantidad)) = map((obtenerHerramienta cantidad nombre).(accessorRango (+1))) $unosNinjas

{-clonesDeSombra: reduce la cantidad de ninjas que se necesitan para una misión
en el mismo número que los clones de sombra creados. ¡El tamaño del equipo no
puede ser menor a 1!-}
clonesDeSombra :: Int->Jutsu
clonesDeSombra unaCantidad unaMision = unaMision{rangoRecomendable= noSeaMenorACero (rangoRecomendable unaMision)-unaCantidad}

noSeaMenorACero :: Int->Int
noSeaMenorACero unaCantidad = max 0 unaCantidad

--fuerzaDeUnCentenar: elimina a todos los enemigos con rango menor a 500.
fuerzaDeUnCentenar :: Jutsu
fuerzaDeUnCentenar unaMision = unaMision{enemigos= eliminarEnemigosBajoRango (enemigos unaMision)}

eliminarEnemigosBajoRango :: [Ninja]->[Ninja]
eliminarEnemigosBajoRango unosNinjas = filter ((500>).rango)  unosNinjas

{-ejecutarMision. Cuando se ejecuta una misión, todos
los ninjas del grupo usan todos sus jutsus en la misión. Luego, si la misión es copada o
factible, se cumple. Caso contrario la misión se falla.-}
ejecutarMision :: [Ninja]->Mision->[Ninja]
ejecutarMision unosNinjas unaMision = (cumplirOFallarMision unosNinjas).(usarJutsus unosNinjas) $unaMision 

cumplirOFallarMision :: [Ninja]->Mision->[Ninja]
cumplirOFallarMision unosNinjas unaMision
    |esCopada unaMision || esFactible unosNinjas unaMision = cumplirMision unosNinjas unaMision
    | otherwise = fallarMision unosNinjas unaMision

usarJutsus :: [Ninja]->Mision->Mision
usarJutsus unosNinjas unaMision = foldr ($) unaMision (map aplicarSusJutsus unosNinjas)

aplicarSusJutsus :: Ninja->Mision->Mision
aplicarSusJutsus unNinja unaMision= foldr ($) unaMision (jutsu unNinja)     

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
granGuerraNinja = UnaMision 100000 100 infinitosZetsus (UnaHerramienta "Honor" 1)

infinitosZetsus :: [Ninja]
infinitosZetsus = map zetsu [1..]

zetsu :: Int-> Ninja
zetsu unNumero = UnNinja ("Zetsu" ++ show unNumero) [] [] 600

{-ghci> esDesafiante infinitosZetsus granGuerraNinja
Interrupted.
ghci> esCopada granGuerraNinja
False
ghci> fuerzaDeUnCentenar granGuerraNinja
UnaMision {cantidadNinjas = 100000, rangoRecomendable = 100, enemigos = Interrupted.-}