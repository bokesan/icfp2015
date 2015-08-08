module Play (run) where

import Rng
import Board

moves :: [Move]
moves = [MoveSW, MoveSE, MoveW, MoveE, RotCW, RotCCW]

nextMove :: Problem -> [Unit] -> Unit -> (Move, Unit)
nextMove problem prev unit
  | not (validPos problem prev unit) = (Stop, unit)
  | atBottom problem unit = (Lock, unit)
  | otherwise = case [(m,u) | m <- moves, let u = move unit m, validPos problem prev u && u /= unit] of
                  [] -> (Lock, unit)
                  (u1:_) -> u1

units :: Problem -> Integer -> [Unit]
units p seed = let rnds = take (pSourceLength p) (randoms seed)
                   us = pUnits p
                   n = length us
               in [us !! (r `rem` n) | r <- rnds]

run :: Problem -> Integer -> [(Move,Unit)]
run p seed = let (u1:us) = units p seed in
             run1 p us [] (spawn p u1) 

run1 :: Problem -> [Unit] -> [Unit] -> Unit -> [(Move,Unit)]
run1 p ether prev u =
  case nextMove p prev u of
    (Stop, _) -> []
    (Lock, u') -> case ether of
                   [] -> [(Lock,u')]
                   (u1:us) -> let u1' = spawn p u1 in (Lock,u1') : run1 (lock p u') us [] u1'
    (m,u') -> (m,u') : run1 p ether (u:prev) u'

spawn :: Problem -> Unit -> Unit
spawn p u = let (min_x,max_x,min_y,_max_y) = dimensions u
                yoffs = 0 - min_y
                width = 1 + max_x - min_x
                space = pWidth p - width
                xoffs = space `quot` 2 - min_x
            in translate u xoffs yoffs
