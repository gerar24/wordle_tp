module Main (main) where

import Core
import System.Exit (exitFailure, exitSuccess)

testMatch :: IO Bool
testMatch = do
    let tests = [ 
            -- Caso 1: Palabras completamente iguales
            (match "hello" "hello", [('h', Correcto), ('e', Correcto), ('l', Correcto), ('l', Correcto), ('o', Correcto)], "Test 1: Palabras iguales"),
            -- Caso 2: Palabras completamente distintas
            (match "hello" "abcdf", [('a', NoPertenece), ('b', NoPertenece), ('c', NoPertenece), ('d', NoPertenece), ('f', NoPertenece)], "Test 2: Palabras distintas"),
            -- Caso 3: Coincidencias parciales
            (match "hello" "ehlol", [('e', LugarIncorrecto), ('h', LugarIncorrecto), ('l', Correcto), ('o', LugarIncorrecto), ('l', LugarIncorrecto)], "Test 3: Coincidencias parciales"),
            -- Caso 4: Algunos correctos, otros no pertenecen
            (match "hello" "hibro", [('h', Correcto), ('i', NoPertenece), ('b', NoPertenece), ('r', NoPertenece), ('o', Correcto)], "Test 4: Correctos y no pertenece")
            ]
    runTests tests

runTests :: [([(Char, Match)], [(Char, Match)], String)] -> IO Bool
runTests [] = return True
runTests ((actual, expected, desc) : rest) = do
    if actual == expected
        then do
            putStrLn $ "✓ " ++ desc
            runTests rest
        else do
            putStrLn $ "✗ " ++ desc
            putStrLn $ "  Esperado: " ++ show expected
            putStrLn $ "  Obtenido: " ++ show actual
            return False




-- Función principal para correr los tests
main :: IO ()
main = do
    allTestsPassed <- testMatch
    if allTestsPassed
        then exitSuccess
        else exitFailure