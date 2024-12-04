{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Game where

import Core

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

--- Clase Juego
data Juego = Juego {
    palSecreta :: String,
    intentosPosibles :: Int,
    intentos :: [[(Char, Match)]]
    } deriving (Generic, ToJSON, FromJSON)

--- Clase Resultado, indica en que situación se encuentra el jugador
data Resultado = Gano | Continua | Perdio
    deriving (Eq, Show)

--- Alias para string de Intento, no confundir con [(Char, Match)]
type Intento = String

--- Genera un nuevo juego con palabra y número de intentos dado.
nuevo :: String -> Int -> Juego
nuevo str n = Juego {palSecreta = str, intentosPosibles = n, intentos = []}

--- Intenta probar un intento, siempre y cuando no termino el juego (Gano o Perdio). PD: Queda medio redundante dado el ciclo del juego.
enviarIntento :: Intento -> Juego -> Maybe Juego
enviarIntento str (Juego pal n ints) = do
    if not(termino (Juego pal n ints) == Continua)
        then Nothing
    else
        return Juego {palSecreta = pal, intentosPosibles = n, intentos = ints ++ [match pal str]}


--- Devuelve la lista de Intentos que ya se realizaron.
getIntentosInfo :: Juego -> [[(Char, Match)]]
getIntentosInfo = intentos

--- Chequea si termino el juego.
termino :: Juego -> Resultado
termino (Juego pal n ints)
    | null ints = Continua -- Sin intentos, continua
    | gano ultimoIntento = Gano -- Ultimo intento realizado coincide completamente, entonces Gano.
    | n == length ints && not (gano ultimoIntento) = Perdio -- Ultimo intento posible y no coincide, entonces Perdio. (Redunda segunda clausula por condicición de arriba, pero doble chequeo...)
    | otherwise = Continua -- Si no Perdi, ni Gane, Continuo
  where
    ultimoIntento = last ints --- Get último intento, si es vacio entonces entro en primer condición de null.

--- Chequea si el resultado de match de un intento coincide completamente.
gano :: [(Char, Match)] -> Bool
gano matcheo = all ((== Correcto) . snd) matcheo

--- Get longitud de palabra secreta
longPalSecreta :: Juego -> Int
longPalSecreta (Juego pal _ _) = length pal

--- Get intentos disponibles
intDisponibles :: Juego -> Int
intDisponibles (Juego _ n ints) = max 0 (n - length ints)

--- Get intentos totales (los iniciales para utilizar, setting del juego)
intTotales :: Juego -> Int
intTotales = intentosPosibles









