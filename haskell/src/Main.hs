module Main (main) where

import Board
import Chant
import Play
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Aeson as J
import Data.Char (toLower)

import System.Environment

main :: IO ()
main = do args <- getArgs
          let (fs, ps, options) = parseArgs args
          -- mapM_ listUnits fs
          solutions <- process ps fs
          B.putStrLn (J.encode (J.toJSON solutions))
          -- Prelude.putStrLn (chant cPHRASES [MoveE, MoveSW, MoveW, RotCW])

data Option = MaxMemory !Int
            | MaxSeconds !Int
            | AvailableCPUs !Int
            | Invalid String
            deriving (Eq, Ord, Show)

parseArgs :: [String] -> ([String], [String], [Option])
parseArgs = go [] [] []
  where go fs ps os []            = (fs, unquote ps, os)
        go fs ps os ("-f":f:args) = go (f:fs) ps os args
        go fs ps os ("-p":w:args) = go fs (w:ps) os args
        go fs ps os ("-t":n:args) = go fs ps (MaxSeconds (read n) : os) args
        go fs ps os ("-m":n:args) = go fs ps (MaxMemory (read n) : os) args
        go fs ps os ("-c":n:args) = go fs ps (AvailableCPUs (read n) : os) args
        go fs ps os (a:args)      = go fs ps (Invalid a : os) args

unquote :: [String] -> [String]
unquote ps = [map Data.Char.toLower p' | p <- ps, let p' = stripQuotes p, p' /= ""]

stripQuotes :: String -> String
stripQuotes = unbracket '\"' '\"'

unbracket :: Eq a => a -> a -> [a] -> [a]
unbracket open close xs = case xs of
                            [] -> []
                            [_] -> xs
                            (x1:xs') | x1 == open && last xs' == close -> init xs'
                                     | otherwise -> xs

process :: [String] -> [String] -> IO [Solution]
process _ [] = return []
process ps (f : args) = do
          problem <- readProblem f
          let xs1 = map (playWithSeed ps problem) (pSourceSeeds problem)
          xs <- process ps args
          return (xs1 ++ xs)

listUnits :: String -> IO ()
listUnits f = do problem <- readProblem f
                 putStrLn ("problem " ++ show (pId problem))
                 TIO.putStrLn (ppBoard problem)
                 mapM_ printUnit (zip [0 ..] (pUnits problem))

printUnit :: (Int, Unit) -> IO ()
printUnit (index, unit) = do putStrLn ("unit " ++ show index)
                             TIO.putStrLn (ppUnit unit)


playWithSeed :: [String] -> Problem -> Integer -> Solution
playWithSeed ps problem seed =
   let moves = play problem seed
       ps' = if null ps then cPHRASES else ps
       s = Solution{sProblemId = pId problem,
                    sSeed = seed,
                    sTag = T.pack "",
                    sSolution = T.pack (chant ps' [m | (m,_) <- moves])}
   in s      

readProblem :: FilePath -> IO Problem
readProblem f = do
  s <- B.readFile f
  let (Just p) = J.decode s
  return p

cPHRASES :: [String]
cPHRASES = ["Ei!",
            "Ia! Ia!",
            "Yuggoth",
            "Necronomicon",
            "Monkeybox",
            "R'lyeh",
            "Planet 10",
            "YogSothoth",
            "Tsathoggua",
            "Yoyodyne",
            "vigintillion",
            "Cthulhu fhtagn!",
            "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn.",
            "In his house at R'lyeh dead Cthulhu waits dreaming.",
            "The Laundry",
            "John Bigboote",
            "BLUE HADES",
            "CASE NIGHTMARE GREEN"]
