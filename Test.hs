import Test.HUnit
import Test.QuickCheck
import Debug.Trace

import Graphics.Gloss

import Game
import Logic
import Visuals

testGame  = Game {board = boardSize 3, player=Player red, state = Running, bs = 3}
winnerGame = testGame {board = winnerBoard 3}


test1 = TestCase $ assertEqual "boardsize 3"  73 (length $ boardSize 3)
test2 = TestCase $ assertEqual "length listOfNeighbours 0 0 == 6" 6 (length $ listOfNeighbours (0,0))
test3 = let (x,y) = (12,11) in
    TestCase $ assertEqual ("extractCoords " ++ show (x,y)) (x,y) (extractCords (Void grey (x,y)))
test4 = let (x,y) = (-0.1,0) in
    TestCase $ assertEqual ("extractCoords " ++ show (x,y)) (x,y) (extractCords (Void grey (x,y)))
test5 = TestCase $ assertEqual "extractColor grey" grey (extractColor (Void grey (0,0)))
test6 = TestCase $ assertEqual "extractColor red" red (extractColor (Marble red (12, 22.51)))
test7 = TestCase $ assertBool "checkwinner red" (checkWinner (Player red) winnerGame)
test8 = TestCase $ assertBool "checkwinner blue" (checkWinner (Player blue) winnerGame)
test9 = TestCase $ assertBool "checkwinner white" (checkWinner (Player white) winnerGame)

--check if a Game is the same after a move is made on the board
test10 = TestCase $ assertBool "move game /= game"
    (move (Marble red (0.0,-262.5)) (Void grey (-75.77722,-131.25)) testGame /= testGame)


 -- Tests Olof ---
 

-- From origo: One diagonal step north east plus one diagonal step north west results in x = 0.
test11 = TestCase $ assertEqual "diagonal steps to x = 0" 0 (fst (head (diagPM (diagPP [(0,0)] 1) 1)))

-- From origo: One diagonal step north east plus one diagonal step south east results in y = 0.
test12 = TestCase $ assertEqual "diagonal steps to y = 0" 0 (snd (head (diagMP (diagPP [(0,0)] 1) 1)))

-- Transposes a list 5 steps to the right, and then 5 steps to the left, results in the original 
test13 = TestCase $ assertEqual "transpose width" (transpWidthM 5 (transpWidthP 5 lst)) lst
    where
        lst = [(0,0),(1,1),(2,2)]



tests = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13]
runtests = runTestTT $ TestList tests



-- Quickcheck --

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


-- QuickChecks Olof ---


-- Checks that the point inversion works for lists of pairs of coordinates.
invertXY :: [(Float,Float)] -> Bool
invertXY lst = trace ("LHS: " ++ show a) a == trace ("RHS: " ++ show b) b
    where
        a = invertY (invertX lst)
        b = [(-z,-w) | (z,w) <- lst]

qCinvertXY :: IO ()
qCinvertXY = quickCheck (withMaxSuccess 200 invertXY)

-- Checks that cell n in the D-line is the same cell as origo transposed diagonally
-- north west n times. Due to the nature of floats, these values may need to be
-- truncated.

dLine :: Positive Int -> Bool
dLine (Positive n) = trace ("a" ++ show a) a == trace ("b" ++ show b) b
    where
        a = (truncate (fst a2), (truncate (snd a2)))
        a2 = head (addDline (fromIntegral n) [])
        b = (truncate (fst b2), (truncate (snd b2)))
        b2 = head (diagPP [(0,0)] (fromIntegral n))

qCdLine :: IO ()
qCdLine = quickCheck (withMaxSuccess 10000 dLine)


















