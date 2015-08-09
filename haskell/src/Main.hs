module Main (main) where

import Board
import Play
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy as T
import Data.Aeson as J

import System.Environment

main :: IO ()
main = do args <- getArgs
          let (fs, ps, options) = parseArgs args
          if ListUnits `notElem` options
            then do
              solutions <- process fs
              B.putStrLn (J.encode (J.toJSON solutions))
              -- mapM_ (\(m,u) -> putStrLn (show m ++ " " ++ show u)) moves
            else
              mapM_ listUnits fs

data Option = ListUnits
            | MaxMemory !Int
            | MaxSeconds !Int
            deriving (Eq, Ord, Show)

parseArgs :: [String] -> ([String], [String], [Option])
parseArgs = go [] [] []
  where go fs ps os [] = (fs, ps, os)
        go fs ps os ("-f":f:args) = go (f:fs) ps os args
        go fs ps os ("-T":args)   = go fs ps (ListUnits:os) args
        go fs ps os ("-t":n:args)   = go fs ps (MaxSeconds (read n) : os) args
        go fs ps os ("-m":n:args)   = go fs ps (MaxMemory (read n) : os) args
        go fs ps os ("-p":w:args) = go fs (w:ps) os args

process :: [String] -> IO [Solution]
process [] = return []
process (f : args) = do
          problem <- readProblem f
          let xs1 = Prelude.map (playWithSeed problem) (pSourceSeeds problem)
          xs <- process args
          return (xs1 ++ xs)

listUnits :: String -> IO ()
listUnits f = do problem <- readProblem f
                 putStrLn ("problem " ++ show (pId problem))
                 mapM_ printUnit (Prelude.zip [0 ..] (pUnits problem))

printUnit :: (Int, Unit) -> IO ()
printUnit (index, unit) = do putStrLn ("unit " ++ show index)
                             putStr (ppUnit unit)


playWithSeed :: Problem -> Integer -> Solution
playWithSeed problem seed =
   let moves = play problem seed
       s = Solution{sProblemId = pId problem,
                    sSeed = seed,
                    sTag = T.pack "",
                    sSolution = T.pack [encodeMove m | (m,_) <- moves]}
   in s      

readProblem :: FilePath -> IO Problem
readProblem f = do
  s <- B.readFile f
  let (Just p) = J.decode s
  return p

encodeMove :: Move -> Char
encodeMove MoveW  = '3'
encodeMove MoveE  = '2'
encodeMove MoveSW = '4'
encodeMove MoveSE = '5'
encodeMove RotCW  = '1'
encodeMove RotCCW = 'k'
encodeMove Lock   = '4'
