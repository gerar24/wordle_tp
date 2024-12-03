module Game where
import Core

data Juego = Juego {
    palSecreta :: String,
    intentosPosibles :: Int,
    intentos :: [[(Char, Match)]]
    }

data Resultado = Gano | Continua | Perdio
    deriving (Eq, Show)

type Intento = String

nuevo :: String -> Int -> Juego
nuevo str n = Juego {palSecreta = str, intentosPosibles = n, intentos = []}

enviarIntento :: Intento -> Juego -> Maybe Juego
enviarIntento str (Juego pal n ints) = do
    if not(termino (Juego pal n ints) == Continua)
        then Nothing
    else
        return Juego {palSecreta = pal, intentosPosibles = n, intentos = ints ++ [match pal str]}

getIntentosInfo :: Juego -> [[(Char, Match)]]
getIntentosInfo = intentos

termino :: Juego -> Resultado
termino (Juego pal n ints)
    | null ints = Continua
    | gano ultimoIntento = Gano
    | n == length ints && not (gano ultimoIntento) = Perdio
    | otherwise = Continua
  where
    ultimoIntento = last ints 

gano :: [(Char, Match)] -> Bool
gano matcheo = all ((== Correcto) . snd) matcheo

longPalSecreta :: Juego -> Int
longPalSecreta (Juego pal _ _) = length pal

intDisponibles :: Juego -> Int
intDisponibles (Juego _ n ints) = n - length ints

intTotales :: Juego -> Int
intTotales = intentosPosibles









