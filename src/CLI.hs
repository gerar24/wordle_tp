module CLI (main) where

import Core
import Game
import System.Environment (getArgs)
import System.IO.Error
import System.Random.Stateful (uniformRM, globalStdGen)
import TinyApp.Interactive
import Data.Char (toUpper, toLower, isLetter)
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
                                runInteractive (wordle (Estado {juego = nuevo palabra 5, intentoActual = [], alertas = [], diccionario=diccInicial, descartadas=Set.empty}))
                        else
                            do
                                putStrLn "The word is not in the dictionary. Please try again."
                                --runInteractive' (wordle (Estado {juego = nuevo "hello" 5, intentoActual = [], alertas = []}))
                Nothing -> do
                                putStrLn "Let's start the game with random word."
                                rWord <- selectRandomWord diccInicial
                                let randomWord = map toLower rWord
                                runInteractive (wordle (Estado {juego = nuevo randomWord 5, intentoActual = [], alertas = [], diccionario=diccInicial, descartadas=Set.empty}))
    pure()

-- Coments para W08
                    -- SI NOTHING DAILY
                    -- SI DAILY DAILY
                    -- SI DAILY + FECHA, DAILY FECHA
                    -- SI PALABRA PALABRA

--- Seleciona palabra random
selectRandomWord :: Diccionario -> IO String
selectRandomWord dicc = do
                            let listaPalabras = vocabulario dicc
                            indice <- uniformRM (0, length listaPalabras - 1) globalStdGen
                            return (listaPalabras !! indice)

--- Carga diccionario
loadDictionary :: FilePath -> IO (Maybe [String])
loadDictionary path = do
                        result <- decodeFileEither path
                        case result of
                                        Right diccionario -> return (Just diccionario)
                                        Left _ -> do
                                                    putStrLn "Error al cargar el diccionario."
                                                    return Nothing

--- Parsea args para modo de run
parseArgs :: [String] -> Maybe String
parseArgs ("--palabra":valor:_) = Just valor
parseArgs _ = Nothing

--- Tipo diccionario
newtype Diccionario = Diccionario {
    vocabulario :: [String]
}

--- Clase de Estado
data Estado = Estado {
    juego :: Juego,
    intentoActual :: String,
    alertas :: [String],
    diccionario :: Diccionario,
    descartadas :: Set.Set Char
}

--- Limpieza de Estado (intento en vivo y alertas que se le muestran al usuario)
cleanState :: Estado -> Estado
cleanState estado = estado {intentoActual=[]}

cleanAlertas :: Estado -> Estado
cleanAlertas estado = estado {alertas=[]}

--- CLI Interactive principal
wordle :: Estado -> Sandbox Estado
wordle estadoInicial =
    Sandbox {
        initialize = estadoInicial,
        --- Dibujamos ;)
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
            <> showAlertas (alertas s)
            <> "\n\n\n\n",

        --- Refrescamos todo el rato...    
        update = \(Key key _) s ->
            case termino (juego s) of
                Gano -> do
                    --- Solo puedo salir
                    if key == KEsc
                        then
                            (s, Exit)
                        else
                            ((cleanAlertas s) {alertas= (alertas (cleanAlertas s)) ++ ["Ganaste crack mundial ;)\nApretá 'Esc' para Salir"]}, Continue)
                Perdio -> do
                            --- Solo puedo salir
                            if key == KEsc
                                then
                                    (s, Exit)
                                else
                                    ((cleanAlertas s) {alertas= (alertas (cleanAlertas s)) ++ ["Perdiste, lo siento mucho :/\nLa palabra era: " ++ map toUpper (palSecreta(juego s)) ++ "\nApretá 'Esc' para Salir"]}, Continue)
                Continua -> case key of 
                            --- Puedo salir (Esc)
                            --- Intentar Enviar Intento (Enter)
                            --- Borrar Letra (KBS)
                            --- Escribir Letra (KChar)
                            --- Cualquier otra cosa (_ Continua como si nada el juego)
                                KEsc -> (cleanState s, Exit)
                                KEnter -> 
                                    --- Chequeo validez de longitud
                                    if longPalSecreta (juego s) == length (intentoActual s)
                                        then
                                            --- Chequeo validez de palabra en diccionario
                                            if map toUpper (intentoActual s) `elem` vocabulario (diccionario s)
                                                then
                                                    --- Chequeo si el juego lo acepta (quedó medio redundante dado ciclo de juego que evita jugar si ya terminó)
                                                    case enviarIntento (intentoActual s) (juego s) of
                                                        Just nuevoJuego -> 
                                                            let estadoActualizado = s { juego = nuevoJuego, 
                                                                                        descartadas = Set.union (descartadas s) (yaDescartadas (last (intentos nuevoJuego)))}
                                                            in (cleanAlertas (cleanState estadoActualizado), Continue)
                                                        Nothing -> (cleanAlertas (cleanState s), Continue)
                                                else
                                                    (s {alertas = (alertas s) ++ ["Esa palabra no es válida."]}, Continue)
                                        else
                                            (s {alertas = (alertas s) ++ ["Longitud inválida."]}, Continue)

                                KBS -> if length (intentoActual s) > 0 --- Puedo borrar solo si hay letras por borrar
                                            then
                                                let estadoActualizado = s {intentoActual = init (intentoActual s)}
                                                in (cleanAlertas estadoActualizado, Continue)
                                            else
                                                (s,Continue)

                                KChar pressed -> if isLetter pressed --- Chequeo validez de input.
                                                    then
                                                        --- Chequeo no pasarme de la "grilla"
                                                        if longPalSecreta (juego s)  > length (intentoActual s)
                                                            then
                                                                --- Chequeo si ya la descarté para dar alerta
                                                                if Set.member pressed (descartadas s)
                                                                    then
                                                                        let estadoActualizado = s {intentoActual = intentoActual s ++ [toLower pressed], alertas = (alertas s) ++ ["Atención! La letra " ++ [toUpper pressed] ++ " ya la descartaste antes..."]}
                                                                        in (estadoActualizado, Continue)
                                                                    else
                                                                        let estadoActualizado = s {intentoActual = intentoActual s ++ [toLower pressed]}
                                                                        in (estadoActualizado, Continue)
                                                            else
                                                                (s, Continue)
                                                    else
                                                        (s {alertas = (alertas s) ++ ["Carácter inválido."]}, Continue) -- Curiosidad, pensé que cuando "carácter" se refería a letra iba sin tilde, pero no es así
                                _ -> (s, Continue) --- Casos que "no nos importan"
                                
    }

--- Mostrar alertas en líneas separadas
showAlertas :: [String] -> String
showAlertas alertas = concatMap (++ "\n") alertas

--- Mostrar letras ya descartadas
showYaDescartadas :: Set.Set Char -> String
showYaDescartadas descartadas =
                                let lista = Set.toList descartadas
                                in unwords (map (\c-> [toUpper c]) lista)

--- Calcular conjunto de letras que ya descarto el jugador
yaDescartadas :: [(Char, Match)] -> Set.Set Char
yaDescartadas [] = Set.empty
yaDescartadas ((char , match):xs)
    | match == NoPertenece = Set.insert char (yaDescartadas xs)
    | otherwise = yaDescartadas xs

--- Pretty prints del juego ;)
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

        

