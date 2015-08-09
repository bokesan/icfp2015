module Play (play) where

import Rng
import Board

import Data.List


play :: Problem -> Integer -> [(Move,Unit)]
play problem seed = concat (h problem allUnits)
   where
      allUnits = units problem seed
      h _ [] = []
      h p (u:us) = case placeUnit u p of
                     (moves, p') -> moves : h p' us

moves :: [Move]
moves = [MoveSW, MoveSE, MoveW, MoveE, RotCW, RotCCW]

nextMove :: Problem -> [Unit] -> Unit -> (Move, Unit)
nextMove problem prev unit
  | not (validPos problem prev unit) = (Stop, unit)
  | atBottom problem unit = (Lock, unit)
  | otherwise = case validMoves problem prev unit of
                  [] -> (Lock, unit)
                  (u1:_) -> u1
  -- TODO: statische Bewertungsfunktion
  --    Tiefe
  --    Anzahl Bewegungsmöglichkeiten
  --    atBottom ersetzen

-- list of units for this seed and sourceLength
units :: Problem -> Integer -> [Unit]
units p seed = let rnds = take (pSourceLength p) (randoms seed)
                   us = pUnits p
                   n = length us
               in [us !! (r `rem` n) | r <- rnds]

placeUnit :: Unit -> Problem -> ([(Move,Unit)], Problem)
placeUnit unit problem = go (spawn problem unit)
   where
      go u = h [] u
             -- case allMoves problem u of
             --   [] -> h [] u
             --   (ms:_) -> let (_,u') = last ms in (ms ++ [(Lock, u')], lock problem u')
      h prev u = case nextMove problem prev u of
                   (Stop, _) -> ([], problem) -- FIXME: how can this happen?
                   (Lock, u') -> ([(Lock,u')], lock problem u')
                   (m,u') -> case h (u:prev) u' of
                               (ms,p) -> ((m,u'):ms, p)


-- center new unit on top of board
spawn :: Problem -> Unit -> Unit
spawn p u = let (min_x,max_x,min_y,_max_y) = dimensions u
                yoffs = 0 - min_y
                width = 1 + max_x - min_x
                space = pWidth p - width
                xoffs = space `quot` 2 - min_x
            in translate u xoffs yoffs


-- ==============================

goto :: Problem -> Unit -> Unit -> [[(Move,Unit)]]
goto problem src dest
   | src `below` dest = []
   | src == dest = [[]]
   | otherwise   = [ (z:ms') | z@(_,src') <- sortBy (distTo dest) (validMoves problem [] src),
                               ms' <- goto problem src' dest ]

distTo :: Unit -> (Move,Unit) -> (Move,Unit) -> Ordering
distTo dest (_,u1) (_,u2) = compare (distance dest u1) (distance dest u2)

validMoves :: Problem -> [Unit] -> Unit -> [(Move, Unit)]
validMoves problem prev unit =
  [(m,u) | m <- moves, let u = move unit m, validPos problem prev u && u /= unit]

-- ===============================

-- nice destinations for a unit :-)
--
-- no idea what to do - just start from lower left
destinations :: Problem -> Unit -> [Unit]
destinations problem unit = [ u' | p <- ps, u <- rotations unit, let u' = tr u p,
                                   validPos problem [] u' ]
  where
    tr u@(Unit (Cell px py) _) (Cell x y) = let dx = x - px; dy = y - py in translate u dx dy
    ps = sortBy cmp (cells problem)
    cmp (Cell x1 y1) (Cell x2 y2) = case compare y2 y1 of
                                      EQ -> compare x1 x2
                                      r  -> r

-- now, generate all moves to all destinations...
allMoves :: Problem -> Unit -> [[(Move,Unit)]]
allMoves problem unit = concat [goto problem unit dest | dest <- destinations problem unit]
