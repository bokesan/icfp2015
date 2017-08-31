{-# LANGUAGE OverloadedStrings #-}
module Board (
       Move(..),
       Cell(..),
       Unit, makeUnit, ppUnit,
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
import qualified Data.Vector.Generic.Mutable as MV
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

cellX, cellY :: Cell -> Int
cellX (Cell x _) = x
cellY (Cell _ y) = y

-- abstract measure of cell distance, no particular unit
cellDistance :: Cell -> Cell -> Int
cellDistance (Cell x1 y1) (Cell x2 y2)
   = sq (x2-x1) + sq (y2-y1)
     where sq x = x * x


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

{-
rotateCW, rotateCCW :: Unit -> Unit
rotateCW  u = uMapMembers (rotCellCW (uPivot u)) u
rotateCCW u = uMapMembers (rotCellCCW (uPivot u)) u
-}

times :: Int -> (a -> a) -> a -> a
n `times` f | n <= 0    = id
            | otherwise = f . ((n - 1) `times` f)



-- members are ordered
-- pivot and rotations. Rotation is empty list if invalid.
data BaseUnit = BaseUnit !Cell (V.Vector [Cell]) deriving (Eq, Ord, Show)

instance Hashable BaseUnit where
    hashWithSalt s (BaseUnit p ms) = s `hashWithSalt` p `hashWithSalt` (ms V.! 0)

-- Possibly confusing performance hack:
-- Since we are never comparing different units, but only translations/rotations
-- of one unit, the BaseUnit part is ignored fpr Eq/Ord/Hashable.
data Unit = Unit !Int !Int !Int !BaseUnit deriving (Show)

instance Eq Unit where
   (Unit x1 y1 r1 _) == (Unit x2 y2 r2 _) = x1 == x2 && y1 == y2 && r1 == r2

instance Ord Unit where
  compare (Unit x1 y1 r1 _) (Unit x2 y2 r2 _) =
    case compare x1 x2 of
      EQ -> case compare y1 y2 of
              EQ -> compare r1 r2
              r' -> r'
      r  -> r
    
uPivot :: Unit -> Cell
uPivot (Unit xoffs yoffs _ (BaseUnit pivot _)) = translateCell xoffs yoffs pivot

uMembers :: Unit -> [Cell]
uMembers (Unit xoffs yoffs rot (BaseUnit _ rs)) = map (translateCell xoffs yoffs) (rs V.! rot)

{-
uMap, uMap', uMapMembers :: (Cell -> Cell) -> Unit -> Unit
uMapMembers f (Unit1 p m1) = Unit1 p (f m1)
uMapMembers f (Unit2 p m1 m2) = let m1' = f m1
                                    m2' = f m2
                                in if m1' <= m2' then Unit2 p m1' m2'
                                                 else Unit2 p m2' m1'
uMapMembers f (Unit3 p m1 m2 m3)
   = let a = f m1
         b = f m2
         c = f m3
     in if a <= b then
           -- a <= b --> a b c - a c b - c a b
           if b <= c then
               Unit3 p a b c
           else if a <= c then
               Unit3 p a c b
           else
               Unit3 p c a b
        else
           -- a > b  --> b a c - b c a - c b a
           if c <= b then
               Unit3 p c b a
           else if a <= c then
               Unit3 p b a c
           else
               Unit3 p b c a
uMapMembers f (Unit p ms) = Unit p (sort (map f ms))

uMap f (Unit1 p m1) = Unit1 (f p) (f m1)
uMap f (Unit2 p m1 m2) = let m1' = f m1
                             m2' = f m2
                         in if m1' <= m2' then Unit2 (f p) m1' m2'
                                          else Unit2 (f p) m2' m1'
uMap f (Unit3 p m1 m2 m3)
   = let a = f m1
         b = f m2
         c = f m3
     in if a <= b then
           -- a <= b --> a b c - a c b - c a b
           if b <= c then
               Unit3 (f p) a b c
           else if a <= c then
               Unit3 (f p) a c b
           else
               Unit3 (f p) c a b
        else
           -- a > b  --> b a c - b c a - c b a
           if c <= b then
               Unit3 (f p) c b a
           else if a <= c then
               Unit3 (f p) b a c
           else
               Unit3 (f p) b c a
uMap f (Unit p ms) = Unit (f p) (sort (map f ms))

uMap' f (Unit1 p m1)    = Unit1 (f p) (f m1)
uMap' f (Unit2 p m1 m2) = Unit2 (f p) (f m1) (f m2)
uMap' f (Unit3 p m1 m2 m3) = Unit3 (f p) (f m1) (f m2) (f m3)
uMap' f (Unit p ms)     = Unit (f p) (map f ms)
-}
            
instance Hashable Unit where
  hashWithSalt s (Unit xo yo r _) = s `hashWithSalt` xo `hashWithSalt` yo
                                      `hashWithSalt` r

makeUnit :: Cell -> [Cell] -> Unit
makeUnit pivot members = Unit 0 0 0 (BaseUnit pivot (V.fromList rotations))
  where
    rot :: [Cell] -> [Cell]
    rot ms = sort (map (rotCellCW pivot) ms)
    ms' = sort members
    rotations = ms' : map validate (tail (take 6 (iterate rot members)))
    validate r = if r == ms' then [] else r

moveUnit :: Unit -> Move -> Unit
moveUnit (Unit xo yo r b) move =
   case move of
      RotCW  -> Unit xo yo ((r+1) `rem` 6) b
      RotCCW -> Unit xo yo ((r+5) `rem` 6) b
      MoveW  -> Unit (xo-1) yo r b
      MoveE  -> Unit (xo+1) yo r b
      MoveSW | even yo   -> Unit xo     (yo+1) r b
             | otherwise -> Unit (xo-1) (yo+1) r b
      MoveSE | even yo   -> Unit xo     (yo+1) r b
             | otherwise -> Unit (xo+1) (yo+1) r b
         
dimensions :: Unit -> (Int, Int, Int, Int)
dimensions unit        = let xs = map cellX ms
                             ys = map cellY ms
                             ms = uMembers unit
                         in (minimum xs, maximum xs, minimum ys, maximum ys)

dimensionsWithPivot :: Unit -> (Int, Int, Int, Int)
dimensionsWithPivot u =
    let (minX, maxX, minY, maxY) = dimensions u
        (Cell px py) = uPivot u
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
        update f = V.modify (\v -> MV.modify v f y) b
         
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
                              return (makeUnit pivot members)
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
u1 `below` u2 = cellY (uPivot u1) > cellY (uPivot u2)

distance :: Unit -> Unit -> Int
distance u1 u2 = cellDistance (uPivot u1) (uPivot u2)

validCell :: Problem -> Cell -> Bool
validCell p cell@(Cell x y) =
    (x >= 0 && x < pWidth p && y >= 0 && y < pHeight p &&
     not (isFilled (pFilled p) cell))

validRotation :: Unit -> Bool
validRotation (Unit _ _ r (BaseUnit _ rs)) = not (null (rs V.! r))
    
validPos :: Problem -> S.HashSet Unit -> Unit -> Bool
validPos p prev u = validRotation u &&
                    all (validCell p) (uMembers u) &&
                    not (u `S.member` prev)

translateCell :: Int -> Int -> Cell -> Cell
translateCell xoffs yoffs (Cell x y)
   | even yoffs = Cell (x+xoffs)   (y+yoffs)
   | odd y      = Cell (x+xoffs+1) (y+yoffs)
   | otherwise  = Cell (x+xoffs-1) (y+yoffs)

translate :: Unit -> Int -> Int -> Unit
translate (Unit xo yo r b) x y = Unit (xo+x) (yo+y) r b

lock :: Problem -> Unit -> Problem
lock problem unit = problem{pFilled = removeFullRows (fillCells (pFilled problem) (uMembers unit))}

atBottom :: Problem -> Unit -> Bool
atBottom problem unit =
  any (\(Cell _ y) -> y >= lastRow) (uMembers unit)
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
rotations (Unit xoffs yoffs _ b@(BaseUnit _ rs)) =
   [ Unit xoffs yoffs r b | (r,ms) <- V.toList (V.indexed rs), not (null ms) ]

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
ppUnit u =
  let (minX, maxX, minY, maxY) = dimensionsWithPivot u
      row y = let r = intersperse ' ' [cell (Cell x y) | x <- [minX..maxX]]
              in if odd y then ' ' : r else r
      cell c | c `elem` ms   = if c == p then cPIVOT_ON_UNIT else cUNIT
             | otherwise     = if c == p then cPIVOT else cEMPTY
      p = uPivot u
      ms = uMembers u
  in
     T.concat (intersperse "\n" [T.pack (row n) | n <- [minY..maxY]])

ppBoard :: Problem -> T.Text
ppBoard p = ppBoardWithUnit p invisibleUnit
  where invisibleUnit = makeUnit (Cell (-1) (-1)) []

ppBoardWithUnit :: Problem -> Unit -> T.Text
ppBoardWithUnit problem unit =
    T.concat (intersperse "\n" [T.pack (alignedRow y) | y <- [0 .. h-1]])
    where
       pivot = uPivot unit
       members = uMembers unit
       w = pWidth problem
       h = pHeight problem
       filled = pFilled problem
       alignedRow y | even y    = row y
                    | otherwise = ' ' : row y
       row y = intersperse ' ' [cell (Cell x y) | x <- [0 .. w-1]]
               
       cell c | isFilled filled c  = if c == pivot then cPIVOT_ON_FILLED else cFILLED
              | c `elem` members   = if c == pivot then cPIVOT_ON_UNIT else cUNIT
              | c == pivot         = cPIVOT
              | otherwise          = cEMPTY
