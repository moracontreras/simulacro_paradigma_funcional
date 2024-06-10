import Text.Show.Functions()


{-Las carreras de autos pueden ser muy divertidas, pero tienen consecuencias. En esta edición de parcial vamos a analizar y producir los efectos que sufren 
los autos al correr una carrera. Los autos se componen de marca, modelo, desgaste (ruedas y chasis, son dos números), velocidad máxima (m/s), y el tiempo de carrera, 
que lo vamos a considerar inicialmente 0 y tendremos en cuenta luego el uso.
Una pista está compuesta de distintas partes (curvas, rectas, boxes), donde cada tramo termina realizando una transformación sobre el auto que la atraviesa.

Nota: Maximizar el uso de aplicación parcial, composición y orden superior. No usar recursividad a menos que se indique que está permitido.

Modelar el auto, teniendo en cuenta la información necesaria que lo representa. Y luego representar:
Auto Ferrari, modelo F50, sin desgaste en su chasis y ruedas, con una velocidad máxima de 65 m/s.
Auto Lamborghini, modelo Diablo, con desgaste 7 de chasis y 4 de ruedas, con una velocidad máxima de 73 m/s.
Auto Fiat, modelo 600, con desgaste 33 de chasis y 27 de ruedas, con una velocidad máxima de 44 m/s.-}

data Auto = UnAuto{
    marca :: String,
    modelo :: String,
    desgaste :: (Ruedas,Chasis),
    velocidadMaxima :: Int,
    tiempo :: Int
}
type Ruedas = Int
type Chasis = Int
type Transformacion = Auto->Auto
type Pista = [Transformacion]

ferrari :: Auto
ferrari = UnAuto "Ferrari" "F50" (0,0) 65 0

lamborghini :: Auto
lamborghini = UnAuto "Lamborghini" "Diablo" (4,7) 73 0

fiat :: Auto
fiat = UnAuto "Fiat" "600" (27,33) 44 0
{-
Estado de salud del auto:
Saber si un auto está en buen estado, esto se da cuando el desgaste del chasis es menor a 40 y el de las ruedas es menor 60.
Saber si un auto no da más, esto ocurre si alguno de los valores de desgaste es mayor a 80.-}

estadoDeSalud :: Auto->String
estadoDeSalud unAuto 
    | desgasteChasis unAuto <40 && desgasteRuedas unAuto <60 = "En buen estado"
    | desgasteChasis unAuto >80 || desgasteRuedas unAuto >80 = "No da más"
    | otherwise = "En mal estado" 

desgasteChasis ::Auto->Chasis    
desgasteChasis unAuto = snd.desgaste $ unAuto

desgasteRuedas :: Auto->Ruedas
desgasteRuedas unAuto = fst.desgaste $ unAuto

modificarChasis :: (Chasis->Chasis)->Auto->Auto
modificarChasis unaFuncion unAuto = unAuto{desgaste = (desgasteRuedas unAuto,unaFuncion.desgasteChasis $ unAuto)}

modificarRuedas :: (Ruedas->Ruedas)->Auto->Auto
modificarRuedas unaFuncion unAuto = unAuto{desgaste = (unaFuncion.desgasteRuedas $ unAuto, desgasteChasis unAuto)}

{-Reparar un Auto: la reparación de un auto baja en un 85% el desgaste del chasis (es decir que si está en 50, lo baja a 7.5)
y deja en 0 el desgaste de las ruedas.-}

reparar :: Auto->Auto
reparar unAuto = modificarRuedas (const 0).modificarChasis (porcentaje 85) $ unAuto 

porcentaje :: Int->Int->Int
porcentaje unPorcentaje unValor = div (unValor*unPorcentaje) 100

{-Modelar las funciones para representar las distintas partes de una pista, teniendo en cuenta:
La curva tiene dos datos relevantes: el ángulo y la longitud. Al atravesar una curva, el auto sufre un desgaste añadido al 
que tenía anteriormente en sus ruedas, que responde a la siguiente cuenta: 3 * longitud / ángulo. 
Además, suma un tiempo de longitud / ( velocidad máxima / 2 )
Modelar curvaPeligrosa, que es una curva de ángulo 60 y longitud de 300m
Modelar curvaTranca, que es una curva de ángulo 110 y longitud de 550m-}

curva :: Int->Int->Transformacion
curva angulo longitud unAuto = sumarTiempoCurva longitud .modificarRuedasCurva angulo longitud $ unAuto

modificarRuedasCurva :: Int->Int->Transformacion
modificarRuedasCurva angulo longitud unAuto = modificarRuedas (+ div (3*longitud) angulo) unAuto

sumarTiempoCurva :: Int->Transformacion
sumarTiempoCurva longitud unAuto = modificarTiempo (+ div longitud (div (velocidadMaxima unAuto) 2)) unAuto

modificarTiempo :: (Int->Int)->Auto->Auto
modificarTiempo unaFuncion unAuto = unAuto{tiempo = unaFuncion.tiempo $ unAuto}

curvaPeligrosa :: Transformacion
curvaPeligrosa unAuto = curva 60 300 unAuto

curvaTranca :: Transformacion
curvaTranca unAuto = curva 110 550 unAuto

{-El tramo recto, debido a la alta velocidad se afecta el chasis del auto en una centésima parte de la longitud del tramo. 
Además suma un tiempo de longitud / velocidad máxima
Modelar tramoRectoClassic de 750m
Modelar tramito de 280m-}

tramoRecto :: Int->Transformacion
tramoRecto longitud unAuto = sumarTiempoRecto longitud.modificarChasis (subtract (porcentaje 10 longitud)) $ unAuto

sumarTiempoRecto :: Int->Auto->Auto
sumarTiempoRecto longitud unAuto = modificarTiempo (+ div longitud (velocidadMaxima unAuto)) unAuto

tramoRectoClassic :: Transformacion
tramoRectoClassic unAuto = tramoRecto 750 unAuto

tramito :: Transformacion
tramito unAuto = tramoRecto 280 unAuto

{-Cuando pasa por un tramo Boxes, si está en buen estado, solo pasa por el tramo que compone Boxes, 
en el caso contrario se repara y suma 10 segundos de penalización al tiempo del tramo.-}

tramoBoxes :: Transformacion->Transformacion
tramoBoxes unTramo unAuto 
    | (== "En buen estado").estadoDeSalud $ unAuto = unTramo unAuto
    | otherwise = sumarTiempoBoxes.reparar $ unAuto   

sumarTiempoBoxes ::Auto->Auto
sumarTiempoBoxes unAuto = modificarTiempo (+10) unAuto

{-Ya sea por limpieza o lluvia a veces hay una parte de la pista (cualquier parte) que está mojada. 
Suma la mitad de tiempo agregado por el tramo.-}

mojado :: Transformacion->Transformacion
mojado unTramo unAuto= modificarTiempo (+ div (diferenciaTiempo unTramo unAuto ) 2).unTramo $ unAuto

diferenciaTiempo :: Transformacion->Auto->Int
diferenciaTiempo unTramo unAuto = (tiempo.unTramo $ unAuto )- (tiempo unAuto) 

{-Algunos tramos tienen ripio (sí... está un poco descuidada la pista) y produce el doble de efecto de un tramo normal equivalente, 
y se tarda el doble en atravesarlo también. Nos aclaran que, por suerte, nunca hay boxes con ripio.-}

ripio :: Transformacion->Transformacion
ripio unTramo unAuto= unTramo.unTramo $ unAuto

{-Los tramos que tienen alguna obstrucción, además, producen un desgaste en las ruedas de acuerdo a la porción de pista ocupada, 
siendo 2 puntos de desgaste por cada metro de pista que esté ocupada, producto de la maniobra que se debe realizar al esquivar dicha obstrucción.
Nota: Atención con la repetición de lógica en este punto.-}

obstruccion :: Int->Transformacion->Transformacion
obstruccion longitud unTramo unAuto = modificarRuedas (subtract (2*longitud)).unTramo $ unAuto

{-Realizar la función pasarPorTramo/2 que, dado un tramo y un auto, hace que el auto atraviese el tramo, siempre y cuando no pase que no da más.-}

pasarPorTramo :: Transformacion->Transformacion
pasarPorTramo unTramo unAuto 
    | (estadoDeSalud unAuto) /= "No da mas" = unTramo unAuto
    | otherwise = unAuto

{-Teniendo en cuenta que una pista es una sucesión de tramos: 
Crear la superPista con los siguientes tramos:
tramoRectoClassic
curvaTranca
2 tramitos consecutivos, pero el primero está mojado
Curva con ángulo de 80º, longitud 400m; con obstrucción de 2m
Curva con ángulo de 115º, longitud 650m
Tramo recto de 970m
curvaPeligrosa
tramito con ripio
Boxes con un Tramo Recto de 800m-}

superPista :: Pista
superPista = [tramoRectoClassic,curvaTranca,mojado tramito,tramito,obstruccion 2 (curva 80 400), curva 115 650, tramoRecto 970, curvaPeligrosa, ripio tramito,tramoBoxes (tramoRecto 800)]

{-Hacer la función peganLaVuelta/2 que dada una pista y una lista de autos, hace que todos los autos den la vuelta 
(es decir, que avancen por todos los tramos), teniendo en cuenta que un auto que no da más “deja de avanzar”.-}

daVuelta :: Pista->Transformacion
daVuelta [] unAuto = unAuto
daVuelta (tramo1:resto) unAuto
    | not.noDaMas $ unAuto = daVuelta resto . tramo1 $ unAuto
    | otherwise = unAuto

noDaMas :: Auto->Bool
noDaMas unAuto = (estadoDeSalud unAuto) == "No da mas"

daVueltaBien :: Pista->Transformacion
daVueltaBien unaPista unAuto = foldr pasarPorTramo unAuto unaPista     
-- pasarPorTramo ya considera opcion no da mas

darVueltaAutos :: Pista->[Auto]->[Auto]
darVueltaAutos unaPista unosAutos = filter (not.noDaMas ).map (daVueltaBien unaPista) $ unosAutos

{-¡¡Y llegaron las carreras!!
Modelar una carrera que está dada por una pista y un número de vueltas.
Representar el tourBuenosAires, una carrera que se realiza en la superPista y tiene 20 vueltas.
Hacer que una lista de autos juegue una carrera, teniendo los resultados parciales de cada vuelta, 
y la eliminación de los autos que no dan más en cada vuelta.
-}

data Carrera = UnaCarrera{
    pista :: Pista,
    vueltas :: Int
}

tourBuenosAires :: Carrera
tourBuenosAires = UnaCarrera superPista 20

correr :: [Auto]->Carrera->[[Auto]]
correr unosAutos unaCarrera = take (vueltas unaCarrera).iterate (darVueltaAutos.pista $ unaCarrera) $ unosAutos