import Test.HUnit
import Test.QuickCheck
import Debug.Trace

import Graphics.Gloss

import Game
import Logic
import Visuals

testGame  = Game {board = testboard, player=Player red, state = Running}



test1 = TestCase $ assertEqual "boardsize 3"  73 (length $ boardSize 3)
test2 = TestCase $ assertEqual "length listOfNeighbours 0 0 == 6" 6 (length $ listOfNeighbours (0,0))
test3 = let (x,y) = (12,11) in
    TestCase $ assertEqual ("extractCoords " ++ show (x,y)) (x,y) (extractCords (Void grey (x,y)))
test4 = let (x,y) = (-0.1,0) in
    TestCase $ assertEqual ("extractCoords " ++ show (x,y)) (x,y) (extractCords (Void grey (x,y)))
test5 = TestCase $ assertEqual "extractColor grey" grey (extractColor (Void grey (0,0)))
test6 = TestCase $ assertEqual "extractColor red" red (extractColor (Marble red (12, 22.51)))
test7 = TestCase $ assertBool "checkwinner red" (checkWinner (Player red) winnerBoard)
test8 = TestCase $ assertBool "checkwinner blue" (checkWinner (Player blue) winnerBoard)
test9 = TestCase $ assertBool "checkwinner white" (checkWinner (Player white) winnerBoard)

--check if a Game is the same after a move is made on the board
test10 = TestCase $ assertBool "move game /= game"
    (move (Marble red (0.0,-262.5)) (Void grey (-75.77722,-131.25)) testGame /= testGame)



tests = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]
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


