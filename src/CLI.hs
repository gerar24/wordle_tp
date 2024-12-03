module CLI (main) where

import Core
import Game
import System.Environment (getArgs)
import System.IO.Error
import System.Random.Stateful (uniformRM, globalStdGen)
import TinyApp.Interactive
import Data.Char (toUpper, toLower)
import Data.Yaml (decodeFileEither)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Game (Juego(palSecreta), intDisponibles)

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
                                putStrLn "Let's start the game with selected word."
                                runInteractive (wordle (Estado {juego = nuevo palabra 5, intentoActual = [], alertas = "", diccionario=diccInicial, descartadas=Set.empty}))
                        else
                            do
                                putStrLn "The word is not in the dictionary. Please try again."
                                --runInteractive' (wordle (Estado {juego = nuevo "hello" 5, intentoActual = [], alertas = ""}))
                Nothing -> do
                    putStrLn "Let's start the game with random word."
                    rWord <- selectRandomWord diccInicial
                    let randomWord = map toLower rWord
                    runInteractive (wordle (Estado {juego = nuevo randomWord 5, intentoActual = [], alertas = "", diccionario=diccInicial, descartadas=Set.empty}))
 -- SI NOTHING DAILY
 -- SI DAILY DAILY
 -- SI DAILY + FECHA, DAILY FECHA
 -- SI PALABRA PALABRA

    pure()

selectRandomWord :: Diccionario -> IO String
selectRandomWord dicc = do
    let listaPalabras = vocabulario dicc
    indice <- uniformRM (0, length listaPalabras - 1) globalStdGen
    return (listaPalabras !! indice)

    

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
    diccionario :: Diccionario,
    descartadas :: Set.Set Char
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
            <> "Letras ya descartadas: " ++ showYaDescartadas (descartadas s)
            <> "\n"
            <> "Intentos restantes: " ++ show (intDisponibles(juego s))
            <> "\n\n"
            <> alertas s
            <> "\n\n\n\n",
            
        update = \(Key key _) s ->
            case termino (juego s) of
                Gano -> do
                    if key == KEsc
                        then
                            (cleanState s, Exit)
                    else
                        (s {alertas="Ganaste crack mundial ;)\nApretá 'Esc' para Salir"}, Continue)
                Perdio -> do
                    if key == KEsc
                        then
                            (cleanState s, Exit)
                        else
                            (s {alertas="Perdiste, lo siento mucho :/\nLa palabra era: " ++ map toUpper (palSecreta(juego s)) ++ "\nApretá 'Esc' para Salir"}, Continue)
                Continua -> case key of
                                KEsc -> (cleanState s, Exit)
                                KEnter -> 
                                    if longPalSecreta (juego s) == length (intentoActual s)
                                        then
                                            if map toUpper (intentoActual s) `elem` vocabulario (diccionario s)
                                                then
                                                    case enviarIntento (intentoActual s) (juego s) of
                                                        Just nuevoJuego -> 
                                                            let estadoActualizado = s { juego = nuevoJuego, 
                                                                                        descartadas = Set.union (descartadas s) (yaDescartadas (last (intentos nuevoJuego)))}
                                                            in (cleanState estadoActualizado, Continue)
                                                        Nothing -> (cleanState s, Continue)
                                            else
                                                (s {alertas = "Esa palabra no es válida."}, Continue)
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

showYaDescartadas :: Set.Set Char -> String
showYaDescartadas descartadas =
    let lista = Set.toList descartadas
    in unwords (map (\c-> [toUpper c]) lista)


yaDescartadas :: [(Char, Match)] -> Set.Set Char
yaDescartadas [] = Set.empty
yaDescartadas ((char , match):xs)
    | match == NoPertenece = Set.insert char (yaDescartadas xs)
    | otherwise = yaDescartadas xs

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

        

