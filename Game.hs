module Game where

import Graphics.Gloss.Data.Color
import Graphics.Gloss

purple :: Color
purple = makeColorI 128 0 128 255
grey :: Color
grey = makeColorI 105 105 105 255

screenWidth :: Int
screenWidth = 700
screenHeight :: Int
screenHeight = 700

cellSize :: Float
cellSize = fromIntegral screenWidth/70
cellWidth :: Float
cellWidth = sqrt 3 * cellSize
cellHeight :: Float
cellHeight = 2 * cellSize

data Cell = Void Point | Marble Color Point deriving (Eq, Show)

type Board = [Cell]

newtype Player = Player Color deriving (Eq, Show)

data GamePlayers = PlayerRed 


data GameState = Running | GameOver | ShowingMoves Cell deriving (Eq, Show)

data Game = Game { board :: Board, player :: Player, state :: GameState} deriving (Eq, Show)

testboard = [Marble green (0, s*3), Marble blue (-w*1.5 ,s*1.5), Void (-(w/2),s*1.5), Void (w/2, s*1.5),
  Marble yellow (w*1.5, s*1.5), Void (-w, 0), Void (0, 0), Void (w, 0), Marble purple (-w*1.5, -s*1.5),
  Void (-(w/2), -s*1.5), Void (w/2, -s*1.5), Marble orange (w*1.5, -s*1.5), Marble red (0, -s*3)] 
    where
        w = cellWidth
        s = cellSize 



--- Modular board

cross :: Int -> [(Float,Float)]
cross n = [((fromIntegral x),(fromIntegral y)) | (x,y) <- (vert n ++ horiz n)]

vertZip n 
  | n >= 2 = zip (replicate (3+2*(n-2)) 0) [(-3)*(n-1),0+(-3)*(n-2)..3*(n-1)]
  | n == 1 = zip (replicate 3 0) [(-3),0..3]
  | n == 0 = []

vert n = [z | z <- (vertZip n), z /= (0,0)]

horiz n = zip [((-1)-n),(0-n)..1+n] (replicate (3+2*n) 0)

startlist :: Int -> [(Float,Float)]
startlist n
  | n == 0 = zip [(-0.5)] [1.5]
  | n == 1 = zip [(-0.5),0] [1.5,3]
  | n >= 2 = zip [(-0.5),0..0.5*((fromIntegral n)-1)] [1.5,3..1.5+1.5*(fromIntegral n)]

xAddOne [(x,y)] = [(x+1,y)]
xAddOne lst =  z ++ xAddOne (init z)
    where
        z = [(x+1,y) | (x,y) <- lst]

getIndices [(x,y)] = [(x+1,y)]
getIndices lst =  last(z) : getIndices (init z)
    where
        z = [(x+1,y) | (x,y) <- lst]

fork n 
  | n == 2 = [(0.5,4.5)]
  | n > 2 = fork ((fromIntegral n)-1) ++ diminisher z
    where
        z = [((0.5+(0.5*((fromIntegral n)-2))),(4.5+(1.5*((fromIntegral n)-2))))]

diminisher [(x,y)]
  | (x-1) == 0 = [(x,y)]
  | (x-1) == (-0.5) = [(x,y)]
  | (x-1) > 0 = [(x,y)] ++ diminisher [((x-1),y)]

getTriTR n = if n <= 1 then (xAddOne (startlist n)) else (xAddOne (startlist n)) ++ fork n
getTriTL n = invertX (getTriTR n)
getTriBL n = invertY (getTriTL n)
getTriBR n = invertX (getTriBL n)
triangles n = (getTriTR n) ++ (getTriTL n) ++ (getTriBL n) ++ (getTriBR n)

getcY n = xAddOne (getIndices (startlist n))
getcO n = invertY (getcY n)
getcP n = invertX (getcO n)
getcB n = invertY (getcP n)
getcG n = transpose (getcP n) n
getcR n = invertY (getcG n)

invertX lst = [(-x,y) | (x,y) <- lst]
invertY lst = [(x,-y) | (x,y) <- lst]

transpose lst n = [(x+1.5+(1*(fromIntegral n)),y+4.5+(3*(fromIntegral n))) | (x,y) <- lst]

{- coordinatesToEncoder
-}
cTe :: [(Float,Float)] -> String -> [((Float,Float),String)]
cTe lst "grey" = [(z,"") | z <- lst]
cTe lst "yellow" = [(z,"cY") | z <- lst]
cTe lst "orange" = [(z,"cO") | z <- lst]
cTe lst "purple" = [(z,"cP") | z <- lst]
cTe lst "blue" = [(z,"cB") | z <- lst]
cTe lst "green" = [(z,"cG") | z <- lst]
cTe lst "red" = [(z,"cR") | z <- lst]

encodedLst n = cTe ((cross n) ++ (triangles n)) "grey" ++ cTe (getcY n) "yellow" ++ cTe (getcO n) "orange" ++ cTe (getcP n) "purple" ++ cTe (getcB n) "blue" ++ cTe (getcG n) "green" ++ cTe (getcR n) "red"

encoderToCell :: [((Float,Float),String)] -> [Cell]
encoderToCell [((f1,f2),"")] = [Void (f1*cellWidth,f2*cellSize)]
encoderToCell [((f1,f2),c)] = [Marble (strToColor c) (f1*cellWidth,f2*cellSize)]
encoderToCell (((f1,f2),""):fs) = Void (f1*cellWidth,f2*cellSize) : encoderToCell fs
encoderToCell (((f1,f2),c):fs) = Marble (strToColor c) (f1*cellWidth,f2*cellSize) : encoderToCell fs

strToColor :: [Char] -> Color
strToColor "cR" = red
strToColor "cO" = orange
strToColor "cP" = purple
strToColor "cY" = yellow
strToColor "cB" = blue
strToColor "cG" = green

boardSize n = encoderToCell (encodedLst (n-1))

--- End of modular board



initialGame = Game {board = (boardSize 11), player= Player red, state = Running}

{-

       g     
b   0    0    y
  0    0   0
p    0   0    o
       r


[[c],
,[c,c,c,c]
,[c,c,c]
,[c,c,c,c]
,[c]]

-}
