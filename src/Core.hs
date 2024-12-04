{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Core where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

--- Clase de matcheo que puede ser una de las siguientes 3.
data Match = NoPertenece | LugarIncorrecto | Correcto
    deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

--- Función que dado un string objetivo y un intento, devuelve lista de tupla (char, match) indicando a que clase Match corresponde.
match :: String -> String -> [(Char,Match)]
match objetivo intento = combinarListas listaCorrectos listaPertenece
    where
        listaCorrectos = chequeoPosCorrecta objetivo intento -- Solo distingo por match perfecto (mas estricto)
        listaPertenece = chequeoParcialCompleto objetivo intento -- Solo distinto por pertenencia

--- Combina listas de manera que me quedo con la restricción más fuerte: NoPertenece < LugarIncorrecto < Correcto
combinarListas :: [(Char,Match)] -> [(Char,Match)] -> [(Char,Match)] -- Me quedo con mayor restriccion
combinarListas [] [] = []
combinarListas ((x1,x2):xs) ((y1,y2):ys)
    | x2 >= y2 = (x1, x2) : combinarListas xs ys
    | otherwise = (y1, y2) : combinarListas xs ys

--- Devuelve lista de tuplas de modo que es: char,Correcto si coincide en posición y char,NoPertenece si no matchea la posición
chequeoPosCorrecta :: String -> String -> [(Char,Match)] -- Distingo entre "verdes" o "naranjas y rojos"
chequeoPosCorrecta [] [] = []
chequeoPosCorrecta (x:xs) (y:ys)
    | x==y = (y, Correcto):chequeoPosCorrecta xs ys
    | otherwise =  (y, NoPertenece):chequeoPosCorrecta xs ys -- Podría ser que pertenezca ... (combListas)

--- Devuelve lista de tuplas de modo que es: char,LugarIncorrecto si almenos pertenece a la palbra objetivo y char,NoPertenece si no está.
chequeoParcialCompleto :: String -> String -> [(Char,Match)] -- Distingo entre "verdes y naranjas" o "rojos"
chequeoParcialCompleto objetivo [] = []
chequeoParcialCompleto objetivo (y:ys)
    | pertenece y objetivo = (y, LugarIncorrecto) : chequeoParcialCompleto objetivo ys -- Podria ser correcto... (combListas)
    | otherwise = (y, NoPertenece) :  chequeoParcialCompleto objetivo ys

--- Devuelve True si un Char pertenece al string dado y False si no.
pertenece :: Char -> String -> Bool -- Char pertenece a String?
pertenece a [] = False
pertenece a (x:xs) =  (a == x) || (pertenece a xs) --hlint me recomienda foldr :)