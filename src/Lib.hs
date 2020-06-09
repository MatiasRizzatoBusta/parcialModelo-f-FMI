module Lib where
import Text.Show.Functions
laVerdad = True
--Matias Rizzato Busta. legajo: 167.782-2

-------------------------------------------- Punto 1 --------------------------------------------
data Pais = UnPais{
    pbi :: Float,
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
reducirXPuestosPublicos reduccion pais = pais{pobActivaPublico = (pobActivaPublico pais) - reduccion, pbi = reduccionPuestosPublicos pais}

reduccionPuestosPublicos :: Pais->Float
reduccionPuestosPublicos pais | ((>100000).pobActivaPublico) pais = ((*0.8).pobActivaPublico) pais
                              |otherwise = ((*0.75).pobActivaPublico) pais

entregarExplotacion :: Recurso->Receta
entregarExplotacion recurso pais = pais{recursosNaturales = expropioRecurso recurso (recursosNaturales pais),deuda =(deuda pais) - 2}

expropioRecurso :: Recurso->[Recurso]->[Recurso]
expropioRecurso recurso = filter (not.(== recurso))

blindaje :: Receta
blindaje pais = pais{pbi = (pbi pais) + (((*0.5).pbi) pais),pobActivaPublico = (pobActivaPublico pais) - 500 }
