module Game where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

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
cellWidth = sqrt(3) * cellSize
cellHeight :: Float
cellHeight = 2 * cellSize

data Cell = Void | Marble Color deriving (Show)

data Player = PlayerRed 
    | PlayerBlue 
    | PlayerGreen 
    | PlayerYellow 
    | PlayerOrange 
    | PlayerPurple deriving Show


data Game = Game { board :: Board, player :: Player} deriving Show

type Board = [Cell]

testboard = [Marble green, Marble blue, Void, Void, Marble yellow, Void, Void, Void, Marble purple, Void, Void, Marble orange,Marble red] 

initialGame = Game {board = testboard, player= PlayerBlue}
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