import Test.HUnit
import Test.QuickCheck
import Debug.Trace

import Game
import Logic
import Visuals





test1 = TestCase $ assertEqual "boardsize 3"  73 (length $ boardSize 3)
test2 = TestCase $ assertEqual "length listOfNeighbours 0 0 == 6" 6 (length $ listOfNeighbours (0,0))
test3 = let (x,y) = (12,11) in
    TestCase $ assertEqual ("extractCoords " ++ show (x,y)) (x,y) (extractCords (Void grey (x,y)))
test4 = let (x,y) = (0.1,0) in
    TestCase $ assertEqual ("extractCoords " ++ show (x,y)) (x,y) (extractCords (Void grey (x,y)))



tests = [test1, test2, test3, test4]
runtests = runTestTT $ TestList tests


--Quickcheck

{- cellCount n
    Formula for calculating the number of cells in a hexagonal grid of size n
-}
cellCount :: Int -> Int
cellCount n = 6*n*n + 6*n + 1

{-cellCountProp n
    quickheck property for checking that the number of cells in 
    a board is the expected value.

-}
cellCountProp :: Positive Int -> Bool
cellCountProp (Positive n) = length (boardSize n) == cellCount n

{-celLCountTest
    runs the cellCountProp test 1000 times
-}
cellCountTest :: IO ()
cellCountTest = quickCheck (withMaxSuccess 1000 cellCountProp)


