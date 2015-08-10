module Main(main) where

import Rng

units24 :: String
units24 = ("I........." ++
           ".........." ++
           "2..C...---" ++
           "--F--5----" ++
           "---1-__P__" ++
           "___0")

main = putStrLn (map takeChar (take 1620 (randoms 18)))

takeChar :: Int -> Char
takeChar n = units24 !! (n `rem` length units24)
