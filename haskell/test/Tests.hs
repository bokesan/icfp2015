module Main where

import Board
import Rng
import Chant


import Test.QuickCheck
import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.List


instance Arbitrary Cell where
    arbitrary = do x <- arbitrary
                   y <- arbitrary
                   return $ Cell x y

instance Arbitrary Unit where
    arbitrary = do pivot <- arbitrary
                   members <- arbitrary
                   return $ Unit pivot (sort members)

movesCancel :: Move -> Move -> Unit -> Bool
movesCancel m1 m2 unit = moveUnit (moveUnit unit m1) m2 == unit

sixTimes :: Move -> Unit -> Bool
sixTimes rot unit = (iterate (\u -> moveUnit u rot) unit !! 6) == unit

translateBF :: Unit -> Int -> Int -> Bool
translateBF u x y = translate (translate u x y) (-x) (-y) == u
         
main :: IO ()
main = defaultMain tests



tests :: [TF.Test]
tests = [
        testGroup "QuickCheck Unit" [
                testProperty "rotate R/L"  (movesCancel RotCW RotCCW),
                testProperty "rotate L/R"  (movesCancel RotCCW RotCW),
                testProperty "move M+E"  (movesCancel MoveW MoveE),
                testProperty "move E+W"  (movesCancel MoveE MoveW),
                testProperty "round CW"  (sixTimes RotCW),
                testProperty "round CCW"  (sixTimes RotCCW),
                testProperty "translate" translateBF
                ],
        testGroup "Point tests Unit" [
                testCase "simpleMove" ((moveUnit (Unit (Cell 1 2) [Cell 4 5]) MoveE)
                                       @?= (Unit (Cell 2 2) [Cell 5 5])),

                -- check random number sequence matches problem specification
                testCase "random" (assertBool "rng ok"
                                     ([0,24107,16552,12125,9427,13152,21440,3383,6873,16117]
                                      `isPrefixOf` randoms 17)),

                testCase "chant" (chant ["Ei!"] [RotCW, MoveE, MoveSW, MoveW] @?= "dEi!")
                ]
       ]
