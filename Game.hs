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
cellSize = fromIntegral screenWidth/20
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


--- 6-marble board

boardSize6m = encoderToCell (coordToEncoder (zip [(-3)..3] (replicate 7 0)) "b1" ++ coordToEncoder (zip [(-3.5),(-2.5)..3.5] (replicate 8 1.5)) "b2" ++ coordToEncoder (zip [(-3.5),(-2.5)..3.5] (replicate 8 (-1.5))) "b3" ++ coordToEncoder (zip [(-4)..4] (replicate 9 3)) "b4" ++ coordToEncoder (zip [(-4)..4] (replicate 9 (-3))) "b5" ++ coordToEncoder (zip [(-4.5),(-3.5)..4.5] (replicate 10 4.5)) "b6" ++ coordToEncoder (zip [(-4.5),(-3.5)..4.5] (replicate 10 (-4.5))) "b7" ++ coordToEncoder [(0,6),(1,6),(0,9),(0.5,7.5),((-1),6),((-0.5),7.5)] "b8" ++ coordToEncoder [(0,(-6)),(1,(-6)),(0,(-9)),(0.5,(-7.5)),((-1),(-6)),((-0.5),(-7.5))] "b9")

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

coordToEncoder :: [(Float,Float)] -> String -> [((Float,Float),String)]
coordToEncoder lst "b1" = zip lst (replicate (length lst) "")
coordToEncoder (f:fs) "b2" = (f,"cB") : (last(fs),"cY") : coordToEncoder (init fs) "b1"
coordToEncoder (f:fs) "b3" = (f,"cP") : (last(fs),"cO") : coordToEncoder (init fs) "b1"
coordToEncoder (f1:f2:fs) "b4" = (f1,"cB") : (f2,"cB") : (last(fs),"cY") : (last(init(fs)),"cY") : coordToEncoder (init(init(fs))) "b1"
coordToEncoder (f1:f2:fs) "b5" = (f1,"cP") : (f2,"cP") : (last(fs),"cO") : (last(init(fs)),"cO") : coordToEncoder (init(init(fs))) "b1"
coordToEncoder [f1,f2,f3,f4,f5,f6,f7,f8,f9,f10] "b6" = [(f1,"cB"),(f2,"cB"),(f3,"cB"),(f4,""),(f5,""),(f6,""),(f7,""),(f8,"cY"),(f9,"cY"),(f10,"cY")]
coordToEncoder [f1,f2,f3,f4,f5,f6,f7,f8,f9,f10] "b7" = [(f1,"cP"),(f2,"cP"),(f3,"cP"),(f4,""),(f5,""),(f6,""),(f7,""),(f8,"cO"),(f9,"cO"),(f10,"cO")]
coordToEncoder lst "b8" = zip lst (replicate (length lst) "cG")
coordToEncoder lst "b9" = zip lst (replicate (length lst) "cR")

--- end of 6-marble board


initialGame = Game {board = boardSize6m, player= Player red, state = Running}
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
