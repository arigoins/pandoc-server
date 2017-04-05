module Main where

import           Lib

main :: IO ()
main = putStrLn "Starting Noteloom Pandoc Server..."
       >> startApp
