{-# LANGUAGE OverloadedStrings #-}
module Board (
       Move(..),
       Cell(..),
       Unit(..), ppUnit,
       Problem(pWidth, pHeight, pUnits, pSourceLength, pSourceSeeds, pId),
       cells, ppBoard, ppBoardWithUnit,
       Solution(..),
       moveUnit, validPos, dimensions, translate, below, distance,
       lock, atBottom, rotations
  ) where

import Control.Monad
import qualified Data.Text.Lazy as T
import Data.Aeson
import Data.Bits
import Data.List
import qualified Data.Vector as V
import qualified Data.HashSet as S
import Data.Hashable

data Move = MoveE | MoveSE | MoveSW | MoveW
          | RotCW | RotCCW
          | Lock | Stop
          deriving (Eq, Ord, Enum, Show)



data Cell = Cell !Int !Int deriving (Eq, Ord)

instance Show Cell where
  showsPrec _ (Cell x y) = showChar '(' . shows x . showChar ',' . shows y . showChar ')'

instance Hashable Cell where
  hashWithSalt s (Cell x y) = s `hashWithSalt` x `hashWithSalt` y

-- abstract measure of cell distance, no particular unit
cellDistance :: Cell -> Cell -> Int
cellDistance (Cell x1 y1) (Cell x2 y2)
   = sq (x2-x1) + sq (y2-y1)
     where sq x = x * x

evenRow :: Cell -> Bool
evenRow (Cell _ y) = even y

moveCell :: Cell -> Move -> Cell
moveCell cell | evenRow cell = moveCellE cell
              | otherwise    = moveCellO cell

moveCellE :: Cell -> Move -> Cell
moveCellE (Cell x y) move = case move of
                              MoveE  -> Cell (x+1) y
                              MoveSE -> Cell x (y+1)
                              MoveSW -> Cell (x-1) (y+1)
                              MoveW  -> Cell (x-1) y
                              _ -> undefined

moveCellO :: Cell -> Move -> Cell
moveCellO (Cell x y) move = case move of
                              MoveE  -> Cell (x+1) y
                              MoveSE -> Cell (x+1) (y+1)
                              MoveSW -> Cell x (y+1)
                              MoveW  -> Cell (x-1) y
                              _ -> undefined

type Cube = (Int,Int,Int)

cubeToOffset :: Cube -> Cell
cubeToOffset (x,_,z) = Cell (x + ((z - (z .&. 1)) `div` 2)) z

offsetToCube :: Cell -> Cube
offsetToCube (Cell cx cy) =
  let x = cx - ((cy - (cy .&. 1)) `div` 2)
      z = cy
      y = (-x) - z
  in (x,y,z)

rotCellCW :: Cell -> Cell -> Cell
rotCellCW center point =
    let (ccx, ccy, ccz) = offsetToCube center
        (cpx, cpy, cpz) = offsetToCube point
        dx = cpx - ccx
        dy = cpy - ccy
        dz = cpz - ccz
        p = (ccx - dz, ccy - dx, ccz - dy)
    in cubeToOffset p

rotCellCCW :: Cell -> Cell -> Cell
rotCellCCW center point =
    let (ccx, ccy, ccz) = offsetToCube center
        (cpx, cpy, cpz) = offsetToCube point
        dx = cpx - ccx
        dy = cpy - ccy
        dz = cpz - ccz
        p = (ccx - dy, ccy - dz, ccz - dx)
    in cubeToOffset p


rotateCW, rotateCCW :: Unit -> Unit
rotateCW  (Unit pivot ms) = Unit pivot (sort [rotCellCW pivot cell | cell <- ms])
rotateCCW (Unit pivot ms) = Unit pivot (sort [rotCellCCW pivot cell | cell <- ms])

times :: Int -> (a -> a) -> a -> a
n `times` f | n <= 0    = id
            | otherwise = f . ((n - 1) `times` f)



-- members are ordered
data Unit = Unit !Cell [Cell]
            deriving (Eq, Ord, Show)

instance Hashable Unit where
  hashWithSalt s (Unit p ms) = s `hashWithSalt` p `hashWithSalt` ms

moveUnit :: Unit -> Move -> Unit
moveUnit u RotCW  = rotateCW u
moveUnit u RotCCW = rotateCCW u
moveUnit (Unit p ms) move = Unit (moveCell p move) (sort [moveCell c move | c <- ms])

dimensions :: Unit -> (Int, Int, Int, Int)
dimensions (Unit _ ms) = let xs = [x | Cell x _ <- ms]
                             ys = [y | Cell _ y <- ms]
                         in (minimum xs, maximum xs, minimum ys, maximum ys)

dimensionsWithPivot :: Unit -> (Int, Int, Int, Int)
dimensionsWithPivot u@(Unit (Cell px py) _) =
    let (minX, maxX, minY, maxY) = dimensions u
    in (min minX px, max maxX px, min minY py, max maxY py)
                            
data Problem = Problem {
                 pId :: !Int,
                 pUnits :: [Unit],
                 pWidth, pHeight :: !Int,
                 pFilled :: !Board,
                 pSourceLength :: !Int,
                 pSourceSeeds :: [Integer]
               }
               deriving (Show)

data Board = Board !Int !Int !(V.Vector Integer) deriving (Show)

isFilled :: Board -> Cell -> Bool
isFilled (Board _ _ b) (Cell x y) = testBit (b V.! y) x

setFilled :: Board -> Cell -> Board
setFilled (Board w h b) (Cell x y) = Board w h (update setCell)
  where setCell row = setBit row x
        update f = b V.// [(y, f (b V.! y))]
         
fillCells :: Board -> [Cell] -> Board
fillCells b cs = foldl' setFilled b cs

makeBoard :: Int -> Int -> Board
makeBoard w h = Board w h (V.replicate h 0)
         
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
                                              pFilled = fillCells (makeBoard w h) f,
                                              pSourceLength=len,
                                              pSourceSeeds=seeds})
    parseJSON _ = mzero


below :: Unit -> Unit -> Bool
Unit (Cell _ r1) _ `below` Unit (Cell _ r2) _ = r1 > r2

distance :: Unit -> Unit -> Int
distance (Unit p1 _) (Unit p2 _) = cellDistance p1 p2

validCell :: Problem -> Cell -> Bool
validCell p cell@(Cell x y) =
    (x >= 0 && x < pWidth p && y >= 0 && y < pHeight p &&
     not (isFilled (pFilled p) cell))

validPos :: Problem -> S.HashSet Unit -> Unit -> Bool
validPos p prev u@(Unit _ ms) = all (validCell p) ms && not (u `S.member` prev)

translate :: Unit -> Int -> Int -> Unit
translate (Unit p ms) xoffs yoffs =
  let tr  (Cell x y) = Cell (x+xoffs) (y+yoffs)
      tr' (Cell x y) | odd y     = Cell (x+xoffs+1) (y+yoffs)
                     | otherwise = Cell (x+xoffs-1) (y+yoffs)
  in
     if even yoffs then Unit (tr p) (map tr ms)
                   else Unit (tr' p) (sort (map tr' ms))
 
lock :: Problem -> Unit -> Problem
lock problem (Unit _ ms) = problem{pFilled = removeFullRows (fillCells (pFilled problem) ms)}

atBottom :: Problem -> Unit -> Bool
atBottom problem (Unit _ ms) =
  any (>= lastRow) [y | (Cell _ y) <- ms]
  where lastRow = pHeight problem - 1

isFullRow :: Board -> Int -> Bool
isFullRow (Board w _ cs) r = (cs V.! r) == (shift 1 w - 1)
  
removeFullRows :: Board -> Board
removeFullRows b@(Board _ height _) = foldr remove b [0 .. height - 1]
  where
    remove row cs | isFullRow cs row = remRow row cs
                  | otherwise = cs
    remRow row (Board w h cs) = Board w h (V.cons 0 (V.take row cs V.++ V.drop (row + 1) cs))

rotations :: Unit -> [Unit]
rotations u = nub [ (n `times` rotateCW) u | n <- [0..5] ]

cells :: Problem -> [Cell]
cells problem = [ Cell x y | x <- [0..lastCol], y <- [0..lastRow] ]
  where lastCol = pWidth problem - 1
        lastRow = pHeight problem - 1


cEMPTY, cFILLED, cUNIT, cPIVOT, cPIVOT_ON_UNIT, cPIVOT_ON_FILLED :: Char
cEMPTY         = '·'
cFILLED        = '#'
cUNIT          = '*'
cPIVOT_ON_UNIT = '@'
cPIVOT_ON_FILLED = '§'
cPIVOT         = '•'

                  
ppUnit :: Unit -> T.Text
ppUnit u@(Unit p ms) =
  let (minX, maxX, minY, maxY) = dimensionsWithPivot u
      row y = let r = intersperse ' ' [cell (Cell x y) | x <- [minX..maxX]]
              in if odd y then ' ' : r else r
      cell c | c `elem` ms = if c == p then cPIVOT_ON_UNIT else cUNIT
             | otherwise   = if c == p then cPIVOT else cEMPTY
  in
     T.concat (intersperse "\n" [T.pack (row n) | n <- [minY..maxY]])

ppBoard :: Problem -> T.Text
ppBoard p = ppBoardWithUnit p invisibleUnit
  where invisibleUnit = Unit (Cell (-1) (-1)) []

ppBoardWithUnit :: Problem -> Unit -> T.Text
ppBoardWithUnit problem (Unit pivot members) =
    T.concat (intersperse "\n" [T.pack (alignedRow y) | y <- [0 .. h-1]])
    where
       w = pWidth problem
       h = pHeight problem
       filled = pFilled problem
       alignedRow y | even y    = row y
                    | otherwise = ' ' : row y
       row y = intersperse ' ' [cell (Cell x y) | x <- [0 .. w-1]]
               
       cell c | isFilled filled c = if c == pivot then cPIVOT_ON_FILLED else cFILLED
              | c `elem` members  = if c == pivot then cPIVOT_ON_UNIT else cUNIT
              | c == pivot        = cPIVOT
              | otherwise         = cEMPTY
