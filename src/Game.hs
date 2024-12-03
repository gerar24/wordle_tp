module Game where
import Core

data Juego = Juego {
    palSecreta :: String,
    intentosPosibles :: Int,
    intentos :: [[(Char, Match)]]
    }

type Intento = String

nuevo :: String -> Int -> Juego
nuevo str n = Juego {palSecreta = str, intentosPosibles = n, intentos = []}

enviarIntento :: Intento -> Juego -> Maybe Juego
enviarIntento str (Juego pal n ints) = do
    if termino (Juego pal n ints)
        then Nothing
    else
        return Juego {palSecreta = str, intentosPosibles = n, intentos = ints ++ [match pal str]}

getIntentosInfo :: Juego -> [[(Char, Match)]]
getIntentosInfo = intentos

termino :: Juego -> Bool
termino (Juego _ n ints) = n == length ints

longPalSecreta :: Juego -> Int
longPalSecreta (Juego pal _ _) = length pal

intDisponibles :: Juego -> Int
intDisponibles (Juego _ n ints) = n - length ints

intTotales :: Juego -> Int
intTotales = intentosPosibles









