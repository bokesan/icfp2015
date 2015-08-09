module Main (main) where

import Board
import Play
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy as T
import Data.Aeson as J

import System.Environment

main :: IO ()
main = do args <- getArgs
          solutions <- process args
          B.putStrLn (J.encode (J.toJSON solutions))
          -- mapM_ (\(m,u) -> putStrLn (show m ++ " " ++ show u)) moves

process :: [String] -> IO [Solution]
process [] = return []
process ("-f" : f : args) = do
          problem <- readProblem f
          let xs1 = Prelude.map (playWithSeed problem) (pSourceSeeds problem)
          xs <- process args
          return (xs1 ++ xs)
process (('-':_) : _ : args) = process args


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
