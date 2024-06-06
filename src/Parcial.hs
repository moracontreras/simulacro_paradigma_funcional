module Parcial where
import Text.Show.Functions()

doble :: Int -> Int
doble = (*2)

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

usarHerramienta :: Ninja->Herramienta->Ninja
usarHerramienta unNinja unaHerramienta = (eliminarHerramienta unNinja).nombreHerramienta $unaHerramienta

eliminarHerramienta :: Ninja->String->Ninja
eliminarHerramienta unNinja nombreHerramienta = accessorHerramientas unNinja (sacarHerramienta nombreHerramienta)

sacarHerramienta :: String->[Herramienta]->[Herramienta]
sacarHerramienta unNombre unasHerramientas = filter ((/=unNombre).nombreHerramienta) unasHerramientas

data Mision = UnaMision{
    cantidadNinjas :: Int,
    rangoRecomendable :: Int,
    enemigos :: [Ninja],
    recompensa :: Herramienta
} deriving Show

esDesafiante :: [Ninja]->Mision->Bool
esDesafiante unosNinjas unaMision = any ((rangoRecomendable unaMision>).rango) $unosNinjas

esCopada :: Mision->Bool
esCopada unaMision = any (== recompensa unaMision) [armarHerramienta 3 "bomba de humo", armarHerramienta 5 "shurikens", armarHerramienta 14 "kunais"] 

esFactible :: [Ninja]->Mision->Bool
esFactible unosNinjas unaMision = not (esDesafiante unosNinjas unaMision) && cumpleFactibilidad unosNinjas unaMision

cumpleFactibilidad :: [Ninja]->Mision->Bool
cumpleFactibilidad unosNinjas (UnaMision cantidadEquipo _ _ _) =length unosNinjas >= cantidadEquipo || (sum.map cantidadTotalHerramientas $unosNinjas) >500 

fallarMision :: [Ninja]->Mision->[Ninja]
fallarMision unosNinjas (UnaMision _ rangoSugerido _ _) = (map (disminuirRango 2)).filter (cumpleRango rangoSugerido) $ unosNinjas

cumpleRango :: Int->Ninja->Bool
cumpleRango unRango unNinja= unRango <= rango unNinja

disminuirRango :: Int -> Ninja -> Ninja
disminuirRango unaCantidad unNinja = accessorRango (subtract unaCantidad) unNinja

accessorRango :: (Int->Int)->Ninja->Ninja
accessorRango unaFuncion unNinja = unNinja{rango = unaFuncion.rango $unNinja}

cumplirMision :: [Ninja]->Mision->[Ninja]
cumplirMision unosNinjas (UnaMision _ _ _ (UnaHerramienta nombre cantidad)) = map((obtenerHerramienta cantidad nombre).(accessorRango (+1))) $unosNinjas

clonesDeSombra :: Int->Jutsu
clonesDeSombra unaCantidad unaMision = unaMision{rangoRecomendable= noSeaMenorACero (rangoRecomendable unaMision)-unaCantidad}

noSeaMenorACero :: Int->Int
noSeaMenorACero unaCantidad = max 0 unaCantidad

fuerzaDeUnCentenar :: Jutsu
fuerzaDeUnCentenar unaMision = unaMision{enemigos= eliminarEnemigosBajoRango (enemigos unaMision)}

eliminarEnemigosBajoRango :: [Ninja]->[Ninja]
eliminarEnemigosBajoRango unosNinjas = filter ((500>).rango)  unosNinjas

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