module Game where

import Graphics.Gloss.Data.Color
import Graphics.Gloss

purple :: Color
purple = makeColorI 128 0 128 255
grey :: Color
grey = makeColorI 105 105 105 255

screenWidth :: Int
screenWidth = 800
screenHeight :: Int
screenHeight = 800

cellSize :: Float
cellSize = fromIntegral screenWidth/20
cellWidth :: Float
cellWidth = sqrt 3 * cellSize
cellHeight :: Float
cellHeight = 2 * cellSize

data Cell = Void Point | Marble Color Point deriving (Eq, Show)

type Board = [Cell]

newtype Player = Player Color deriving (Eq, Show)

data Game = Game { board :: Board, player :: Player} deriving (Eq, Show)

testboard = [Marble green (0, s*3), Marble blue (-w*1.5 ,s*1.5), Void (-(w/2),s*1.5), Void (w/2, s*1.5), Marble yellow (w*1.5, s*1.5), Void (-w, 0), Void (0, 0), Void (w, 0), Marble purple (-w*1.5, -s*1.5), Void (-(w/2), -s*1.5), Void (w/2, -s*1.5), Marble orange (w*1.5, -s*1.5), Marble red (0, -s*3)] 
    where
        w = cellWidth
        s = cellSize 
initialGame = Game {board = testboard, player= Player red}
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