import Debug.Trace
import Game
import Graphics.Gloss
import Logic
import Test.HUnit
import Test.QuickCheck
import Visuals

-- interface

testGame = Game {board = boardSize 3, player = Player red, state = Running, bs = 3}

winnerGame = testGame {board = winnerBoard 3}

test1 = TestCase $ assertEqual "boardsize 3" 73 (length $ boardSize 3)

test2 = TestCase $ assertEqual "length listOfNeighbours 0 0 == 6" 6 (length $ listOfNeighbours (0, 0))

test3 =
  let (x, y) = (12, 11)
   in TestCase $ assertEqual ("extractCoords " ++ show (x, y)) (x, y) (extractCords (Void grey (x, y)))

test4 =
  let (x, y) = (-0.1, 0)
   in TestCase $ assertEqual ("extractCoords " ++ show (x, y)) (x, y) (extractCords (Void grey (x, y)))

test5 = TestCase $ assertEqual "extractColor grey" grey (extractColor (Void grey (0, 0)))

test6 = TestCase $ assertEqual "extractColor red" red (extractColor (Marble red (12, 22.51)))

test7 = TestCase $ assertBool "checkwinner red" (checkWinner (Player red) winnerGame)

test8 = TestCase $ assertBool "checkwinner blue" (checkWinner (Player blue) winnerGame)

test9 = TestCase $ assertBool "checkwinner white" (checkWinner (Player white) winnerGame)

--check if a Game is the same after a move is made on the board
test10 =
  TestCase $
    assertBool
      "move game /= game"
      (move (Marble red (0.0, -262.5)) (Void grey (-75.77722, -131.25)) testGame /= testGame)

test11 = let (x,y) = (20.0,-250.5) in 
    TestCase $ assertBool "isInCell" (isInCell (extractCords (Marble red (0.0,-262.5))) (x,y) cellWidth)

test12 = TestCase $ assertEqual "findCell" (Marble green (0,262.5)) (findCell (0,262.5) testboard)

test13 = TestCase $ assertEqual "findOnBoard" [Marble green (0,262.5), 
  Marble blue (-227.33167,131.25)] $ findOnBoard listOfCords testboard
    where
        listOfCords = [(0,262.5),(-227.33167,131.25)]

test14 = TestCase $ assertEqual "neighbours" listOfNeighbours (neighbours (Void grey (0,0)) testboard)
    where
        listOfNeighbours = [Void grey (151.55444,0.0),Void grey (-151.55444,0.0),
          Void grey (75.77722,131.25),Void grey (-75.77722,131.25),
          Void grey (75.77722,-131.25),Void grey (-75.77722,-131.25)]

-- From origo: One diagonal step north east plus one diagonal step north west results in x = 0.
test15= TestCase $ assertEqual "diagonal steps to x = 0" 0 (fst (head (diagPM (diagPP [(0,0)] 1) 1)))

-- From origo: One diagonal step north east plus one diagonal step south east results in y = 0.
test16 = TestCase $ assertEqual "diagonal steps to y = 0" 0 (snd (head (diagMP (diagPP [(0,0)] 1) 1)))

-- A transposition of 4 steps north west equals a transposition of (-4) steps south east.
test17 = TestCase $ assertEqual "diagonal steps comparison" (diagPM [(1,2)] 4) (diagMP [(1,2)] (-4))

-- Transposes a list 5 steps to the right, and then 5 steps to the left, results in the original 
test18 = TestCase $ assertEqual "transpose width" (transpWidthM 5 (transpWidthP 5 lst)) lst
    where
        lst = [(0,0),(1,1),(2,2)]

-- A transposition of 5 steps to the east equals a transposition of (-5) steps to the west.
test19 = TestCase $ assertEqual "transpose width comparison" 
  (transpWidthP 5 [(1,2),(3,4)]) (transpWidthM (-5) [(1,2),(3,4)])


tests = 
  [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10,
  test11, test12, test13, test14, test15, test16, test17, test18, test19]

runtests = runTestTT $ TestList tests


-- Quickcheck --


{- cellCount n
    Formula for calculating the number of cells in a hexagonal grid of size n
-}
cellCount :: Int -> Int
cellCount n = 6 * n * n + 6 * n + 1

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
cellCountTest = quickCheck cellCountProp

{- invertXY lst
    Checks that the point inversion works for lists of pairs of coordinates.
-}
invertXY :: [(Float,Float)] -> Bool
invertXY lst = a ==  b
    where
        a = invertY (invertX lst)
        b = [(-z,-w) | (z,w) <- lst]

qCinvertXY :: IO ()
qCinvertXY = quickCheck (withMaxSuccess 200 invertXY)

{- qCinvertXY
    Checks that cell n in the D-line is the same cell as origo transposed 
    diagonally north west n times. Due to the nature of floats, these 
    values may need to be truncated.
-}
dLine :: Positive Int -> Bool
dLine (Positive n) =  a == b
    where
        a = (truncate (fst a2), (truncate (snd a2)))
        a2 = head (addDline (fromIntegral n) [])
        b = (truncate (fst b2), (truncate (snd b2)))
        b2 = head (diagPP [(0,0)] (fromIntegral n))

qCdLine :: IO ()
qCdLine = quickCheck (withMaxSuccess 10000 dLine)

{- encodedLstTest
    Checks that the length of the list of color encoders resulting from the value n,
    is the same as the amount of cells in a board of size n.
-}
encodedLstTest :: Positive Int -> Bool
encodedLstTest (Positive n) = length (encodedLst n) == 6*(n^2) + 6*n + 1

qCEncodedLstTest :: IO ()
qCEncodedLstTest = quickCheck encodedLstTest



main :: IO ()
main = do
    runtests
    putStrLn "-- Cellcount test --"
    cellCountTest
    putStrLn "-- Invert x y test --"
    qCinvertXY
    putStrLn "-- D-line test --"
    qCdLine
    putStrLn "-- encodedLst test --"
    qCEncodedLstTest
