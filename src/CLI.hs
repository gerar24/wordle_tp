module CLI (main) where

import Core
import Game
import System.Environment (getArgs)
import TinyApp.Interactive
import Data.Char (toUpper, toLower)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just palabra -> do
            putStrLn ("Let's Start the game" <> palabra)
            runInteractive' (wordle (Estado {juego = nuevo palabra 5, intentoActual=[], alertas=""})) --palabra
        --Nothing -> putStrLn "No se eligio palabra"
    
    pure()


parseArgs :: [String] -> Maybe String
parseArgs ("--palabra":valor:_) = Just valor
parseArgs _ = Nothing

data Estado = Estado {
    juego :: Juego,
    intentoActual :: String,
    alertas :: String
}

--nuevoEstado :: String -> Maybe Int -> Estado
--nuevoEstado n = Estado {juego = nuevo (maybe 5 id n), intentoActual = 0}

wordle :: Estado -> Sandbox Estado
wordle estadoInicial =
    Sandbox {
        initialize = estadoInicial, --Juego con 5 intentos definido en Core.hs
        render = \s ->
            --show s
            "\n\n\n"
            <> replicate (4 * length (palSecreta (juego s))) '-' ++ "\n"
            <> showJuego (juego s)
            <> showIntentoActual (intentoActual s)
            <> replicate (4 * length (palSecreta (juego s))) '-'
            <> alertas s,
        update = \(Key key _) s ->
            case termino (juego s) of
                Gano -> do
                    if key == KEsc
                        then
                            (s, Exit)
                    else
                        (s {alertas="YOU WON ;) Press Esc to Exit"}, Continue)
                Perdio -> do
                    if key == KEsc
                        then
                            (s, Exit)
                        else
                            (s {alertas="YOU LOST :/ Press Esc to Exit"}, Continue)
                Continua -> case key of
                                KEsc -> (s, Exit)
                                KEnter -> 
                                    if longPalSecreta (juego s) == length (intentoActual s)
                                        then 
                                            case enviarIntento (intentoActual s) (juego s) of
                                                Just nuevoJuego -> 
                                                    let estadoActualizado = s { juego = nuevoJuego }
                                                    in (estadoActualizado, Continue)
                                                Nothing -> (s, Continue)
                                        else
                                            (s, Continue)

                                KBS -> if length (intentoActual s) > 0
                                            then
                                                let estadoActualizado = s {intentoActual = init (intentoActual s)}
                                                in (estadoActualizado, Continue)
                                        else
                                            (s,Continue)

                                KChar pressed -> if longPalSecreta (juego s)  > length (intentoActual s)
                                                        then
                                                            let estadoActualizado = s {intentoActual = intentoActual s ++ [toLower pressed]}
                                                            in (estadoActualizado, Continue)
                                                else
                                                    (s,Continue)
                                
    }

showJuego :: Juego -> String
showJuego (Juego psec _ ints) =
        case ints of
            [] -> ""
            _ -> concatMap showIntentoPasado ints

showIntentoPasado :: [(Char, Match)] -> String
showIntentoPasado [] = ""
showIntentoPasado intento = 
        concatMap showColor intento ++ "\n" ++ separadorLinea
    where
        ancho = length intento
        separadorLinea = concat (replicate ancho "---o") ++ "\n"
    
showColor :: (Char, Match) -> String
showColor (char, match) =
    let color = case match of
            Correcto -> "\ESC[42m"
            LugarIncorrecto -> "\ESC[103m"
            NoPertenece -> "\ESC[41m"
        resetColor = "\ESC[39m\ESC[49m"--"\ESC[39m\ESC[49m" 
        
    in resetColor ++ color ++ " " ++ [toUpper char] ++ " " ++ color ++ resetColor ++ "|"

showIntentoActual :: String -> String
showIntentoActual [] = ""
showIntentoActual intentoActual =
    concatMap (\c -> " " ++ [toUpper c] ++ " |") intentoActual ++ "\n"
--showIntentoActual intentoActual = map toUpper intentoActual ++ "\n"

        

