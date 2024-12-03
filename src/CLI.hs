module CLI (main) where

import Core
import Game
import System.Environment (getArgs)
import System.IO.Error
import TinyApp.Interactive
import Data.Char (toUpper, toLower)
import Data.Yaml (decodeFileEither)
import Data.Maybe (isJust)

-- Main function
main :: IO ()
main = do
    args <- getArgs
    dinicial <- loadDictionary "src/diccionario.json"
    case dinicial of
        Nothing -> putStrLn "Diccionario vacío..."
        Just dicc -> do
            let diccInicial = Diccionario{vocabulario=dicc}
            case parseArgs args of
                Just palabra ->
                    if map toUpper palabra `elem` (vocabulario diccInicial)
                        then
                            do
                                putStrLn "Let's start the game."
                                runInteractive (wordle (Estado {juego = nuevo palabra 5, intentoActual = [], alertas = "", diccionario=diccInicial}))
                        else
                            do
                                putStrLn "The word is not in the dictionary. Please try again."
                                --runInteractive' (wordle (Estado {juego = nuevo "hello" 5, intentoActual = [], alertas = ""}))
                --Nothing -> putStrLn "No word was chosen."
    pure()

loadDictionary :: FilePath -> IO (Maybe [String])
loadDictionary path = do
    result <- decodeFileEither path
    case result of
        Right diccionario -> return (Just diccionario)
        Left _ -> do
            putStrLn "Error al cargar el diccionario."
            return Nothing

parseArgs :: [String] -> Maybe String
parseArgs ("--palabra":valor:_) = Just valor
parseArgs _ = Nothing

newtype Diccionario = Diccionario {
    vocabulario :: [String]
}

data Estado = Estado {
    juego :: Juego,
    intentoActual :: String,
    alertas :: String,
    diccionario :: Diccionario
}

cleanState :: Estado -> Estado
cleanState estado = estado { alertas = "" }

wordle :: Estado -> Sandbox Estado
wordle estadoInicial =
    Sandbox {
        initialize = estadoInicial,
        render = \s ->
            "\n\n\n"
            <> replicate (4 * length (palSecreta (juego s))) '-' ++ "\n"
            <> showJuego (juego s)
            <> showIntentoActual (intentoActual s)
            <> replicate (4 * length (palSecreta (juego s))) '-'
            <> "\n\n"
            <> alertas s,
        update = \(Key key _) s ->
            case termino (juego s) of
                Gano -> do
                    if key == KEsc
                        then
                            (cleanState s, Exit)
                    else
                        (s {alertas="YOU WON ;) Press Esc to Exit"}, Continue)
                Perdio -> do
                    if key == KEsc
                        then
                            (cleanState s, Exit)
                        else
                            (s {alertas="YOU LOST :/ Press Esc to Exit"}, Continue)
                Continua -> case key of
                                KEsc -> (cleanState s, Exit)
                                KEnter -> 
                                    if longPalSecreta (juego s) == length (intentoActual s)
                                        then
                                            if map toUpper (intentoActual s) `elem` vocabulario (diccionario s)
                                                then
                                                    case enviarIntento (intentoActual s) (juego s) of
                                                        Just nuevoJuego -> 
                                                            let estadoActualizado = s { juego = nuevoJuego }
                                                            in (estadoActualizado, Continue)
                                                        Nothing -> (s, Continue)
                                            else
                                                (s {alertas = "Esa palabra no existe."}, Continue)
                                    else
                                        (cleanState s, Continue)

                                KBS -> if length (intentoActual s) > 0
                                            then
                                                let estadoActualizado = s {intentoActual = init (intentoActual s)}
                                                in (estadoActualizado, Continue)
                                        else
                                            (cleanState s,Continue)

                                KChar pressed -> if longPalSecreta (juego s)  > length (intentoActual s)
                                                        then
                                                            let estadoActualizado = s {intentoActual = intentoActual s ++ [toLower pressed]}
                                                            in (cleanState estadoActualizado, Continue)
                                                else
                                                    (cleanState s, Continue)
                                
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
        resetColor = "\ESC[39m\ESC[49m" --"\ESC[39m\ESC[49m" 
        
    in resetColor ++ color ++ " " ++ [toUpper char] ++ " " ++ color ++ resetColor ++ "|"

showIntentoActual :: String -> String
showIntentoActual [] = ""
showIntentoActual intentoActual =
    concatMap (\c -> " " ++ [toUpper c] ++ " |") intentoActual ++ "\n"

        

