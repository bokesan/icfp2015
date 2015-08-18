module Chant (chant) where

import Board

import Data.Char
import Data.List


chant :: [String] -> [Move] -> [Char]
chant ps = go
  where
    ps' = [(w, map decodeMove w) | w <- sortBy (desc length) ps]
    go [] = ""
    go moves = try ps' moves

    try [] (m:ms) = encodeMove m : go ms
    try ((w,c):ps'') ms | c `isPrefixOf` ms = w ++ go (drop (length c) ms)
                        | otherwise         = try ps'' ms


desc :: (a -> Int) -> a -> a -> Ordering
desc f a b = compare (f b) (f a)

canBeCodedAs :: Move -> Char -> Bool
move `canBeCodedAs` char = char `elem` codes move

codes :: Move -> String
codes MoveE   = "ebcfy2"
codes MoveW   = "!p'.03"
codes MoveSE  = "lmno 5"
codes MoveSW  = "iaghj4"
codes RotCW   = "dqrvz1"
codes RotCCW  = "kstuwx"
codes _       = ""

decodeMove :: Char -> Move
decodeMove c = head [m | m <- [MoveE .. RotCCW], toLower c `elem` codes m]

encodeMove :: Move -> Char
encodeMove Lock = '4'
encodeMove m = head (codes m)


