{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI (main) where


import GHC.Generics (Generic)

import Core
import Game
import Game (Juego(palSecreta, intentos, intentosPosibles), intDisponibles)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO.Error
import System.Random.Stateful (uniformRM, globalStdGen)
import TinyApp.Interactive
import Data.Char (toUpper, toLower, isLetter)
import Data.Yaml (decodeFileEither)
import Data.Aeson (decode, encode, FromJSON, ToJSON, toJSON)
import Data.Maybe (Maybe (Nothing))
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time (getCurrentTime, getCurrentTimeZone, utcToLocalTime, formatTime)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BL




--- Main ;)
main :: IO ()
main = do
        args <- getArgs
        --- Veo que tipo de RunType es...
        case parseArgs args of
            Random -> do
                dinicial <- loadDictionary "src/diccionario.json"
                case dinicial of
                    Nothing -> putStrLn "Diccionario vació..."
                    Just dicc -> do
                        let diccInicial = Diccionario{vocabulario=dicc}
                        rWord <- selectRandomWord diccInicial
                        let randomWord = map toLower rWord
                        estadoFinal <- runInteractive (wordle (Estado {juego = nuevo randomWord maxIntentos, intentoActual = [], alertas = [], diccionario=diccInicial, descartadas=Set.empty}))
                        pure()
            Palabra pal -> do
                dinicial <- loadDictionary "src/diccionario.json"
                case dinicial of
                    Nothing -> putStrLn "Diccionario vació..."
                    Just dicc -> do
                        let diccInicial = Diccionario{vocabulario=dicc}
                        --- Como la palabra es pasada por argumento, debo chequear que esté y pasarla al juego como lowercase.
                        if map toUpper pal `elem` (vocabulario diccInicial)
                            then
                                do
                                    estadoFinal <- runInteractive (wordle (Estado {juego = nuevo (map toLower pal) maxIntentos, intentoActual = [], alertas = [], diccionario=diccInicial, descartadas=Set.empty}))
                                    pure()
                            else --- Caso de error...
                                do
                                    putStrLn "La palabra no esta en el diccionario."
                                    pure()
                        pure()
            Daily Nothing -> do
                dinicial <- loadDictionary "src/diccionario.json"
                case dinicial of
                    Nothing -> putStrLn "Diccionario vació..."
                    Just dicc -> do
                        let diccInicial = Diccionario{vocabulario=dicc}
                        --- Defino día actual y trabajo con eso.
                        current <- currentDay
                        let dateStr = show current
                        estadoInicial <- cargoInicioEstado diccInicial dateStr
                        estadoFinal <- runInteractive' (wordle estadoInicial)
                        guardaEstado estadoFinal dateStr
                        pure()
            Daily (Just str) -> do
                dinicial <- loadDictionary "src/diccionario.json"
                case dinicial of
                    Nothing -> putStrLn "Diccionario vació..."
                    Just dicc -> do
                        let diccInicial = Diccionario{vocabulario=dicc}
                        current <- currentDay
                        --- Si el día es válido lo utilizo, sino trabajo con el día actual.
                        let dateStr = maybe (show current) show (Just str)
                        estadoInicial <- cargoInicioEstado diccInicial dateStr
                        estadoFinal <- runInteractive' (wordle estadoInicial)
                        guardaEstado estadoFinal dateStr
                        pure()  
        pure()

--- Obtiene en IO String el día actual
currentDay :: IO String
currentDay = do
    t <- getCurrentTime
    tz <- getCurrentTimeZone
    let day = localDay (utcToLocalTime tz t)
    pure $ formatTime defaultTimeLocale "%Y-%m-%d" day

-- Cargar un estado (el juego nada más) desde JSON o inicializa uno nuevo, dependiendo el Día
cargoInicioEstado :: Diccionario -> String -> IO Estado
cargoInicioEstado dicc dateStr = do
    let nombreArchivo = "estados/"++dateStr++".json"
    exists <- doesFileExist nombreArchivo
    if exists
        then do
            --- Inicializo desde estado (juego) anterior, y recalculo las descartadas...
            putStrLn $ "Cargando estado desde " ++ dateStr
            content <- BL.readFile nombreArchivo
            case decode content of
                Just juego -> pure $ Estado {   
                                                juego = juego,
                                                intentoActual = [],
                                                alertas = [],
                                                diccionario = dicc,
                                                descartadas = descartadasDeIntentos (intentos juego)
                                            }
                Nothing -> do error "Error al decodificar el archivo JSON."
        else do
            --- Inicialización random (para día que nunca se jugó)
            putStrLn "Inicializando estado estándar..."
            rWord <- selectRandomWord dicc
            pure $ Estado {
                juego = nuevo (map toLower rWord) maxIntentos,
                intentoActual = [],
                alertas = [],
                diccionario = dicc,
                descartadas = Set.empty
            }

--- Simula ser una variable global ;) -> devuelve siempre Int 6
maxIntentos :: Int
maxIntentos = 6

--- Guarda estado en json (tan solo el juego)
guardaEstado :: Estado -> String -> IO ()
guardaEstado estado nombreArchivo = do
    let jsonData = encode (juego estado)
    BL.writeFile ("estados/" ++ nombreArchivo ++ ".json") jsonData
    putStrLn $ "Juego guardado en " ++ nombreArchivo ++ ".json"

--- Seleciona palabra random de un diccionario
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

--- Parsea argumentos para obtener clase RunType -> Modo a jugar.
parseArgs :: [String] -> RunType
parseArgs ("--daily":day:_) = Daily (parseDay day) -- Jugaremos con la fecha indicada
parseArgs ["--daily"] = Daily Nothing -- Jugaremos con la fecha actual        
parseArgs ("--palabra":valor:_) = Palabra valor -- Si hay palabra (puede ser invalida y el error lo handlea el main)
parseArgs ["--palabra"] = Random -- Si no hay Palabra jugamos con una Random
parseArgs ["--random"] = Random -- Jugamos Random
parseArgs [] = Daily Nothing -- Jugamos con fecha actual                                 
parseArgs _ = Daily Nothing -- Jugamos con fecha actual

--- Clase tipo de corrida (dado argumentos)
data RunType
    = Daily (Maybe String)
    | Palabra String
    | Random
    deriving (Eq, Show)

--- Parseo argumento a día y chequeo si el argumento fue valido, si lo fue entonces tengo String, sino Nothing
parseDay :: String -> Maybe String
parseDay s = do
    parsed <- iso8601ParseM s :: Maybe Day
    pure s

--- Tipo diccionario
newtype Diccionario = Diccionario {
    vocabulario :: [String]
} deriving (Generic, ToJSON, FromJSON)

--- Clase de Estado
data Estado = Estado {
    juego :: Juego,
    intentoActual :: String,
    alertas :: [String],
    diccionario :: Diccionario,
    descartadas :: Set.Set Char
} deriving (Generic)


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
            let mensajeGanoPierdo =
                    case termino (juego s) of
                        Gano -> "Ganaste crack mundial ;)\nApretá 'Esc' para Salir"
                        Perdio -> "Perdiste, lo siento mucho :/\nLa palabra era: " ++ map toUpper (palSecreta(juego s)) ++ "\nApretá 'Esc' para Salir"
                        Continua -> ""
            in
                "\n\n\n\n\n\n\n"
                <> "Apreta Espacio para saber cuantas vocales (en total, no únicas) tiene la palabra."
                <> "\n\n"
                <> replicate (4 * length (palSecreta (juego s))) '-' ++ "\n"
                <> showJuego (juego s)
                <> showIntentoActual (intentoActual s)
                <> replicate (4 * length (palSecreta (juego s))) '-'
                <> "\n\n"
                <> "Letras ya descartadas: " ++ showYaDescartadas (descartadas s)
                <> "\n"
                <> "Intentos restantes: " ++ show (intDisponibles(juego s))
                <> "\n\n"
                <> mensajeGanoPierdo --- Lo genero acá y no como alerta para que el update se haga correctamente y no esté a la espera de un key extra.
                <> "\n"
                <> showAlertas (alertas s)
                <> "\n\n\n\n",

        --- Refrescamos todo el rato...    
        update = \(Key key _) s ->
            case termino (juego s) of
                --- Idem cases, podría usar _ (Gano, Pierdo)
                Gano -> do
                            --- Solo puedo salir
                            if key == KEsc
                                then
                                    (s, Exit)
                                else
                                    (s, Continue)
                Perdio -> do
                            --- Solo puedo salir
                            if key == KEsc
                                then
                                    (s, Exit)
                                else
                                    (s, Continue)
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
                                
                                KChar pressed -> if pressed == ' ' --- Dar pista.
                                                    then
                                                        ( s {alertas = (alertas s) ++ ["La palabra secreta tiene -> " ++ show (contarVocales (palSecreta (juego s))) ++ " Vocales"]}, Continue)
                                                    else
                                                        if isLetter pressed --- Chequeo validez de input.
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

--- Contar vocales como pista
contarVocales :: String -> Int
contarVocales palabra = length $ filter esVocal palabra

esVocal :: Char -> Bool
esVocal c = c `elem` ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']

--- Mostrar alertas en líneas separadas
showAlertas :: [String] -> String
showAlertas alertas = concatMap (++ "\n") alertas

--- Mostrar letras ya descartadas
showYaDescartadas :: Set.Set Char -> String
showYaDescartadas descartadas =
                                let lista = Set.toList descartadas
                                in unwords (map (\c-> [toUpper c]) lista)

--- Descartadas de todos los intentos
descartadasDeIntentos :: [[(Char, Match)]] -> Set.Set Char
descartadasDeIntentos intentos =
    foldl (\acumulado intento -> Set.union acumulado (yaDescartadas intento)) Set.empty intentos

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

        

