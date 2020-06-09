module Lib where
import Text.Show.Functions

laVerdad = True

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [ Requisito ]

data Depto = Depto {
ambientes :: Int ,
superficie :: Int ,
precio :: Int ,
barrio :: Barrio
} deriving ( Show , Eq )

data Persona = Persona {
mail :: Mail ,
busquedas :: [ Busqueda ]
}

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
    (ordenarSegun criterio . filter (not . criterio x)) xs ++
    [x] ++
    (ordenarSegun criterio . filter (criterio x)) xs


between cotaInferior cotaSuperior valor = valor <= cotaSuperior && valor >= cotaInferior


deptosDeEjemplo = [
    Depto 3 80 7500 "Palermo" ,
    Depto 1 45 3500 "Villa Urquiza" ,
    Depto 2 50 5000 "Palermo" ,
    Depto 1 45 5500 "Recoleta" ]

--PUNTO 1
{-1.
a. Definir las funciones mayor y menor que reciban una función y dos valores, y retorna
true si el resultado de evaluar esa función sobre el primer valor es mayor o menor que el
resultado de evaluarlo sobre el segundo valor respectivamente.
b. Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de
strings en base a su longitud usando ordenarSegun .-}

mayor ::(Ord a) => (b -> a) -> b -> b -> Bool
mayor funcion valor1 valor2 = funcion valor1 > funcion valor2

menor :: (Ord a) => (b -> a) -> b -> b -> Bool
menor funcion valor1 valor2 = funcion valor1 < funcion valor2

{- B
ordenarSegun (menor length) ["mayra", "patricia","carlos", "guido"]
["guido","mayra","carlos","patricia"]

ordenarSegun (mayor length) ["mayra", "patricia","carlos", "guido"]
["patricia","carlos","guido","mayra"]

-}

--PUNTO 2
{-Definir las siguientes funciones para que puedan ser usadas como requisitos de búsqueda:
a. ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero si
el departamento se encuentra en alguno de los barrios de la lista.

b. cumpleRango que a partir de una función y dos números, indique si el valor retornado
por la función al ser aplicada con el departamento-}

ubicadoEn :: [Barrio] -> Requisito
ubicadoEn barriosDeInteres depto = elem (barrio depto) barriosDeInteres
ubicadoEn' barriosDeInteres depto = flip elem barriosDeInteres . barrio $ depto
ubicadoEn'' barriosDeInteres = flip elem barriosDeInteres . barrio 

cumpleRango :: (Num a, Ord a) => (Depto -> a) -> a -> a -> Requisito
cumpleRango funcion numero1 numero2 depto = between numero1 numero2. funcion $ depto
cumpleRango' funcion numero1 numero2  = between numero1 numero2. funcion 

-- PUNTO 3
{-a. Definir la función cumpleBusqueda que se cumple si todos los requisitos de una
búsqueda se verifican para un departamento dado.

b. Definir la función buscar que a partir de una búsqueda, un criterio de ordenamiento y una
lista de departamentos retorne todos aquellos que cumplen con la búsqueda ordenados
en base al criterio recibido.

c. Mostrar un ejemplo de uso de buscar para obtener los departamentos de ejemplo,
ordenado por mayor superficie, que cumplan con:
■ Encontrarse en Recoleta o Palermo
■ Ser de 1 o 2 ambientes
■ Alquilarse a menos de $6000 por mes-}

--PUNTO A
cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda depto = all (cumpleRequisito depto) 

cumpleRequisito :: Depto -> Requisito -> Bool
cumpleRequisito depto requisito = requisito depto

--PUNTO B

buscar :: Busqueda -> (Depto -> Depto -> Bool) -> [Depto] -> [Depto]
buscar busqueda criterioDeOrdenamiento deptos =
    ordenarSegun criterioDeOrdenamiento . filter (flip cumpleBusqueda busqueda) $ deptos
buscar' busqueda criterioDeOrdenamiento  =
    ordenarSegun criterioDeOrdenamiento . filter (flip cumpleBusqueda busqueda) 

--CONSULTA EN CONSOLA
{- buscar busquedaDeEjemplo (mayor superficie) deptosDeEjemplo 
[Depto {ambientes = 2, superficie = 50, precio = 5000, barrio = "Palermo"},
Depto {ambientes = 1, superficie = 45, precio = 5500, barrio = "Recoleta"}]
CUMPLE CON EL CRITERIO DE ORDENAMIENTO (sup de mayor a menor) Y CON LA BUSQUEDA EJEMPLAR (se queda con palermo y recoleta) -}

--PUNTO C
busquedaDeEjemplo :: Busqueda
busquedaDeEjemplo = [
    ubicadoEn ["Recoleta", "Palermo"],
    cumpleRango ambientes 1 2,
     --(<6000).precio
    cumpleRango precio 0 6000
    ]
