module Play (play) where

import Rng
import Board

import qualified Data.Set as S


play :: Problem -> Integer -> [(Move,Unit)]
play problem seed = concat (h problem allUnits)
   where
      allUnits = units problem seed
      h _ [] = []
      h p (u:us) = case placeUnit u p of
                     (moves, p') -> moves : h p' us

moves :: [Move]
moves = [MoveSW, MoveSE, MoveW, MoveE, RotCW, RotCCW]

nextMove :: Problem -> S.Set Unit -> Unit -> (Move, Unit)
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
      go u = -- h [] u
             case findGoodMove problem u of
                Nothing -> h S.empty u
                Just [] -> ([(Lock, unit)], lock problem unit)
                Just ms -> let (_,u') = last ms in (ms ++ [(Lock, u')], lock problem u')
      h prev u = case nextMove problem prev u of
                   (Stop, _) -> ([], problem) -- FIXME: how can this happen?
                   (Lock, u') -> ([(Lock,u')], lock problem u')
                   (m,u') -> case h (S.insert u prev) u' of
                               (ms,p) -> ((m,u'):ms, p)


-- center new unit on top of board
spawn :: Problem -> Unit -> Unit
spawn p u = let (min_x,max_x,min_y,_max_y) = dimensions u
                yoffs = 0 - min_y
                width = 1 + max_x - min_x
                space = pWidth p - width
                xoffs = space `quot` 2 - min_x
            in translate u xoffs yoffs


validMoves :: Problem -> S.Set Unit -> Unit -> [(Move, Unit)]
validMoves problem prev unit =
  [(m,u) | m <- moves, let u = moveUnit unit m, validPos problem prev u && u /= unit]

-- ===============================

-- nice destinations for a unit :-)
--
-- no idea what to do - just start from lower left
destinations :: Problem -> Unit -> [Unit]
destinations problem unit = [ u' | dy <- [pHeight problem - 1, (pHeight problem - 2) .. 0],
                                   dx <- [(-(pWidth problem - 1)) .. pWidth problem - 1],
                                   u <- rotations unit, let u' = translate u dx dy,
                                   validPos problem S.empty u' ]

-- ===============================


findGoodMove :: Problem -> Unit -> Maybe [(Move,Unit)]
findGoodMove problem unit = go (destinations problem unit)
  where
    go [] = Nothing
    go (d:ds) = case findPath problem d unit of
                  Nothing -> go ds
                  x -> x

findPath :: Problem -> Unit -> Unit -> Maybe [(Move,Unit)]
findPath problem dest src = case findPath' problem dest (S.singleton dest) S.empty src of
                              Left _ -> Nothing
                              Right ms -> Just ms

findPath' :: Problem -> Unit -> S.Set Unit -> S.Set Unit -> Unit -> Either (S.Set Unit) [(Move,Unit)]
findPath' problem dest blacklist prev curr
       | curr `S.member` blacklist = if curr == dest then Right [] else Left blacklist
       | otherwise    = go blacklist (validMoves problem prev curr)
  where
     go dead [] = Left dead
     go dead ((m,u):ms) = case findPath' problem dest dead (S.insert curr prev) u of
                            Right mvs -> Right ((m,u):mvs)
                            Left dead' -> go (S.insert u dead') ms
