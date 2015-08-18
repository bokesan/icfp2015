{-# LANGUAGE OverloadedStrings #-}
module Board (
       Move(..),
       Cell(..),
       Unit(..), ppUnit,
       Problem(..), cells,
       Solution(..),
       move, validPos, dimensions, translate, below, distance,
       lock, atBottom, rotations
  ) where

import Control.Monad
import qualified Data.Text.Lazy as T
import Data.Aeson
import Data.Bits
import Data.List

data Move = MoveE | MoveSE | MoveSW | MoveW
          | RotCW | RotCCW
          | Lock | Stop
          deriving (Eq, Ord, Enum, Show)



data Cell = Cell !Int !Int deriving (Eq, Ord)

instance Show Cell where
  showsPrec _ (Cell x y) = showChar '(' . shows x . showChar ',' . shows y . showChar ')'

-- abstract measure of cell distance, no particular unit
cellDistance :: Cell -> Cell -> Int
cellDistance (Cell x1 y1) (Cell x2 y2)
   = sq (x2-x1) + sq (x2-y1)
     where sq x = x * x

evenRow :: Cell -> Bool
evenRow (Cell _ y) = (y .&. 1) == 0

moveCell :: Cell -> Move -> Cell
moveCell cell | evenRow cell = moveCellE cell
              | otherwise    = moveCellO cell

moveCellE :: Cell -> Move -> Cell
moveCellE (Cell x y) move = case move of
                              MoveE  -> Cell (x+1) y
                              MoveSE -> Cell x (y+1)
                              MoveSW -> Cell (x-1) (y+1)
                              MoveW  -> Cell (x-1) y

moveCellO :: Cell -> Move -> Cell
moveCellO (Cell x y) move = case move of
                              MoveE  -> Cell (x+1) y
                              MoveSE -> Cell (x+1) (y+1)
                              MoveSW -> Cell x (y+1)
                              MoveW  -> Cell (x-1) y

data Cube = Cube !Int !Int !Int

cubeToOffset :: Cube -> Cell
cubeToOffset (Cube x _ z) = Cell (x + ((z - (z .&. 1)) `div` 2)) z

offsetToCube :: Cell -> Cube
offsetToCube (Cell cx cy) =
  let x = cx - ((cy - (cy .&. 1)) `div` 2)
      z = cy
      y = (-x) - z
  in Cube x y z

rotCellCW :: Cell -> Cell -> Cell
rotCellCW center point =
    let (Cube ccx ccy ccz) = offsetToCube center
        (Cube cpx cpy cpz) = offsetToCube point
        (Cube dx dy dz) = Cube (cpx - ccx) (cpy - ccy) (cpz - ccz)
        (Cube rx ry rz) = Cube (-dz) (-dx) (-dy)
        p = Cube (ccx + rx) (ccy + ry) (ccz + rz)
    in cubeToOffset p


rotateCW, rotateCCW :: Unit -> Unit
rotateCW (Unit pivot ms) = Unit pivot (sort [rotCellCW pivot cell | cell <- ms])
rotateCCW = 5 `times` rotateCW

times :: Int -> (a -> a) -> a -> a
n `times` f | n <= 0    = id
            | otherwise = f . ((n - 1) `times` f)



-- members are ordered
data Unit = Unit !Cell [Cell]
            deriving (Eq, Ord, Show)

move :: Unit -> Move -> Unit
move u RotCW  = rotateCW u
move u RotCCW = rotateCCW u
move (Unit p ms) move = Unit (moveCell p move) (sort [moveCell c move | c <- ms])

dimensions :: Unit -> (Int, Int, Int, Int)
dimensions (Unit _ ms) = let xs = [x | Cell x _ <- ms]
                             ys = [y | Cell _ y <- ms]
                         in (minimum xs, maximum xs, minimum ys, maximum ys)

data Problem = Problem {
                 pId :: !Int,
                 pUnits :: [Unit],
                 pWidth, pHeight :: !Int,
                 pFilled :: [Cell],
                 pSourceLength :: !Int,
                 pSourceSeeds :: [Integer]
               }
               deriving (Show)

data Solution = Solution {
                  sProblemId :: !Int,
                  sSeed :: !Integer,
                  sTag :: T.Text,
                  sSolution :: T.Text
                }
                deriving (Show)

instance ToJSON Solution where
  toJSON e =
      object [ "problemId" .= sProblemId e
             , "seed" .= sSeed e
             -- , "tag" .= sTag e
             , "solution" .= sSolution e
             ]

instance FromJSON Cell where
    parseJSON (Object v) = do x <- v .: "x"
                              y <- v .: "y"
                              return (Cell x y)
    parseJSON _ = mzero

instance FromJSON Unit where
    parseJSON (Object v) = do pivot <- v .: "pivot"
                              members <- v .: "members"
                              return (Unit pivot (sort members))
    parseJSON _ = mzero
    

instance FromJSON Problem where
    parseJSON (Object v) = do i <- v .: "id"
                              u <- v .: "units"
                              w <- v .: "width"
                              h <- v .: "height"
                              f <- v .: "filled"
                              len <- v .: "sourceLength"
                              seeds <- v .: "sourceSeeds"
                              return (Problem{pId=i, pUnits=u, pWidth=w, pHeight=h,
                                              pFilled=f, pSourceLength=len,
                                              pSourceSeeds=seeds})
    parseJSON _ = mzero


below :: Unit -> Unit -> Bool
Unit (Cell _ r1) _ `below` Unit (Cell _ r2) _ = r1 > r2

distance :: Unit -> Unit -> Int
distance (Unit p1 _) (Unit p2 _) = cellDistance p1 p2

validCell :: Problem -> Cell -> Bool
validCell p cell@(Cell x y) =
    (x >= 0 && x < pWidth p && y >= 0 && y < pHeight p &&
     cell `notElem` pFilled p)

validPos :: Problem -> [Unit] -> Unit -> Bool
validPos p prev u@(Unit _ ms) = all (validCell p) ms && u `notElem` prev

translate :: Unit -> Int -> Int -> Unit
translate (Unit p ms) xoffs yoffs =
  let tr (Cell x y) | odd yoffs && even y = Cell (x+xoffs-1) (y+yoffs)
                    | odd yoffs           = Cell (x+xoffs+1) (y+yoffs)
                    | otherwise           = Cell (x+xoffs) (y+yoffs)
  in Unit (tr p) (sort (map tr ms))

lock :: Problem -> Unit -> Problem
lock problem (Unit _ ms) = problem{pFilled = removeFullRows problem (ms ++ pFilled problem)}

atBottom :: Problem -> Unit -> Bool
atBottom problem (Unit _ ms) =
  any (>= lastRow) [y | (Cell _ y) <- ms]
  where lastRow = pHeight problem - 1


removeFullRows :: Problem -> [Cell] -> [Cell]
removeFullRows p cs1 = foldr rem cs1 [0 .. pHeight p - 1]
  where
    rem row cs | isFullRow row cs = remRow row cs
               | otherwise = cs
    w = pWidth p
    isFullRow row cs = w == length [c | Cell c r <- cs, r == row]
    remRow row cs = [Cell c (if r < row then r+1 else r) | Cell c r <- cs, r /= row]

rotations :: Unit -> [Unit]
rotations u = [ (n `times` rotateCW) u | n <- [0..5] ]

cells :: Problem -> [Cell]
cells problem = [ Cell x y | x <- [0..lastCol], y <- [0..lastRow] ]
  where lastCol = pWidth problem - 1
        lastRow = pHeight problem - 1

ppUnit :: Unit -> String
ppUnit u@(Unit _ ms) =
  let (minX, maxX, minY, maxY) = dimensions u
      row y = concat [cell x y | x <- [minX..maxX]]
      cell x y | Cell x y `elem` ms = "#"
               | otherwise = " "
  in
     concat [row n ++ "\n" | n <- [minY..maxY]]
