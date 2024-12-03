module Core where
--import GHC.Real (integralEnumFromTo)


data Match = NoPertenece | LugarIncorrecto | Correcto -- Correcto > LugarIncorrecto > NoPertenece
    deriving (Eq, Show, Ord)

match :: String -> String -> [(Char,Match)]
match objetivo intento = combinarListas listaCorrectos listaPertenece
    where
        listaCorrectos = chequeoPosCorrecta objetivo intento -- Solo distingo por match perfecto (mas estricto)
        listaPertenece = chequeoParcialCompleto objetivo intento -- Solo distinto por pertenencia

combinarListas :: [(Char,Match)] -> [(Char,Match)] -> [(Char,Match)] -- Me quedo con mayor restriccion
combinarListas [] [] = []
combinarListas ((x1,x2):xs) ((y1,y2):ys)
    | x2 >= y2 = (x1, x2) : combinarListas xs ys
    | otherwise = (y1, y2) : combinarListas xs ys

chequeoPosCorrecta :: String -> String -> [(Char,Match)] -- Distingo entre "verdes" o "naranjas y rojos"
chequeoPosCorrecta [] [] = []
chequeoPosCorrecta (x:xs) (y:ys)
    | x==y = (y, Correcto):chequeoPosCorrecta xs ys
    | otherwise =  (y, NoPertenece):chequeoPosCorrecta xs ys

chequeoParcialCompleto :: String -> String -> [(Char,Match)] -- Distingo entre "verdes y naranjas" o "rojos"
chequeoParcialCompleto objetivo [] = []
chequeoParcialCompleto objetivo (y:ys)
    | pertenece y objetivo = (y, LugarIncorrecto) : chequeoParcialCompleto objetivo ys -- Podria ser correcto...
    | otherwise = (y, NoPertenece) :  chequeoParcialCompleto objetivo ys

pertenece :: Char -> String -> Bool -- Char pertenece a String?
pertenece a [] = False
pertenece a (x:xs) =  (a == x) || (pertenece a xs) --hlint me recomienda foldr :)