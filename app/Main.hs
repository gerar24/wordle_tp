module Main where

import qualified MyLib (someFunc)

import CLI (main)

main :: IO ()
main = CLI.main