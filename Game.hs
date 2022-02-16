module Game where

import Graphics.Gloss

data Cell = Void | Marble deriving (Show)

data Player = PlayerRed 
    | PlayerBlue 
    | PlayerGreen 
    | PlayerYellow 
    | PlayerOrange 
    | PlayerPurple deriving Show


data Game = Game { board :: Board, player :: Player} deriving Show

type Board = [[Cell]]

cellSize = 40

testboard = [[Void], [Void, Void, Void, Void], [Void, Void, Void], [Void, Void, Void, Void],[Void]] 

initialGame = Game {board = testboard, player= PlayerBlue}

gameAsPicture :: Game -> Picture
gameAsPicture game = boardAsPicture $ board game

boardAsPicture board = undefined
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