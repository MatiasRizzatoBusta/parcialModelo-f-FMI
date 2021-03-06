module Lib where
import Text.Show.Functions
laVerdad = True
--Matias Rizzato Busta. legajo: 167.782-2

-------------------------------------------- Punto 1 --------------------------------------------
data Pais = UnPais{
    ipc :: Float,
    pobActivaPublico :: Float,
    pobActivaPrivado :: Float,
    recursosNaturales :: [Recurso],
    deuda :: Float --en millones
}deriving (Show,Eq)

type Recurso = String
namibia = UnPais 4140 400000 650000 ["mineria","ecoturismo"] 50

-------------------------------------------- Punto 2 --------------------------------------------
type Receta = Pais->Pais

prestarNMillon :: Float->Receta
prestarNMillon prestamo pais = pais{deuda=(deuda pais) + (prestamo *1.5)}

reducirXPuestosPublicos :: Float->Receta
reducirXPuestosPublicos reduccion pais = pais{pobActivaPublico = (pobActivaPublico pais) - reduccion,ipc = reduccionPuestosPublicos pais}

reduccionPuestosPublicos :: Pais->Float
reduccionPuestosPublicos pais | ((>100000).pobActivaPublico) pais = cambioIpc(*0.8) (pobActivaPublico pais)
                              |otherwise = cambioIpc (*0.75) (pobActivaPublico pais)

cambioIpc:: (Float->Float)->Float->Float
cambioIpc funcion poblacionActiva =  funcion poblacionActiva

entregarExplotacion :: Recurso->Receta
entregarExplotacion recurso pais = pais{recursosNaturales = expropioRecurso recurso (recursosNaturales pais),deuda =(deuda pais) - 2}

expropioRecurso :: Recurso->[Recurso]->[Recurso]
expropioRecurso recurso = filter (not.(== recurso))

blindaje :: Receta
blindaje pais = pais{ipc = (ipc pais) + cambioIpc (*2) (ipc pais),pobActivaPublico = (pobActivaPublico pais) - 500 }

-------------------------------------------- Punto 3 --------------------------------------------
prestamoPorExplotacion :: Receta
prestamoPorExplotacion  = entregarExplotacion "mineria".prestarNMillon 200 

-------------------------------------------- Punto 4 --------------------------------------------
puedenZafar :: [Pais]->[Pais]
puedenZafar = filter (elem "petroleo".recursosNaturales) --tomo un pais me fijo si en sus recursos esta petroleo y si esta lo tomo

deudaAFavor :: [Pais]->Float
deudaAFavor = sum.map deuda

{-
Composicion y orden superior aparece cada vez que se utiliza un . y me permite hacerle a una variable,o una lista de variables 
en este caso, mas de una funcion sucesivamente.La aplicacion de composicion me permite pasar parametros mediante point free,es decir
sin declararlo en las variables que va a utilizar.
Aplicacion parcial fue utilizado en el filter cuando hago elem "petroleo" ya que me permite componer la condicion que utiliza el 
filter. Usar composicion parcial hace que al funcion espere menos parametros que la original.
El efecto colateral no se puede lograr. Lo que se hace, es hacer una funcion que "copia" los valores originales y cambia estos valores
en la copia.Entonces al hacer composicion, las funciones que le siguen van a trabajar con esta copia y no con los valores ya cargados
-}

-------------------------------------------- Punto 5 --------------------------------------------
estaOrdenadaDePeorAMejor :: Pais->[Receta]->Bool
estaOrdenadaDePeorAMejor pais (x:y:xs) = (pbi.x) pais < (pbi.y) pais && estaOrdenadaDePeorAMejor pais (y:xs)

pbi :: Pais->Float
pbi pais = ((*ipc pais).sumoPobActiva) pais

sumoPobActiva :: Pais->Float
sumoPobActiva pais = (pobActivaPrivado pais) + (pobActivaPrivado pais)

-------------------------------------------- Punto 6 --------------------------------------------
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos -- le agrega energia al listado de recursos naturales infinitas
--veces

{-
si a este pais,le aplicamos la funcion puedenZafar la funcion no va a terminar de filtrar nunca.Esto se debe a que 
recursosNaturalesInfinitos genero una lista infinita de energia en los recursos y el filter nunca va a encontrar un petroleo o 
el fin de lista que para que termine.
La funcion deudaAFavor si funciona con este pais ya que como haskell usa el lazy-evaluation, nunca va a acceder a la lista de recursos
del pais y va a ir directo a lo que cada pais de la lista tenga en el campo deuda para luego sumarlo.
-}
