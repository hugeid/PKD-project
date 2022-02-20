module Visuals where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Game




hexagon :: Float -> Float -> Color -> Picture
hexagon x y c = pictures [color c $ polygon $ hexaCorners x y cellSize, color white $ line $ hexaCorners x y cellSize]

p1 = hexagon 0 0 red

hexaCorners :: Float -> Float -> Float -> Path
hexaCorners x y size = [(x, y-size), (x + w/2, y-size/2), (x + w/2, y+size/2), (x, y+size), (x - w/2, y+size/2), (x - w/2, y-size/2), (x, y-size)]
    where
        w = sqrt 3*size


makeGrid x y = pictures [hexagon x (y+s*3) green, hexagon (x-w*1.5) (y+s*1.5) blue, hexagon (x-w/2) (y+s*1.5) grey,hexagon (x+w/2) (y+s*1.5) grey, hexagon (x+w*1.5) (y+s*1.5) yellow, hexagon (x-w) y grey, hexagon x y grey, hexagon (x+w) y grey,  hexagon (x-w*1.5) (y-s*1.5) purple, hexagon (x-w/2) (y-s*1.5) grey, hexagon (x+w/2) (y-s*1.5) grey, hexagon (x+w*1.5) (y-s*1.5) orange, hexagon x (y-s*3) red]
    where
        w = cellWidth
        s = cellSize
cellCoords x y = [(x, y+s*3), (x-w*1.5 ,y+s*1.5), (x-(w/2),y+s*1.5), (x+w/2, y+s*1.5), (x+w*1.5, y+s*1.5), (x-w, y), (x, y), (x+w, y), (x-w*1.5, y-s*1.5), (x-(w/2), y-s*1.5), (x+w/2, y-s*1.5), (x+w*1.5, y-s*1.5), (x, y-s*3)]
    where
        w = cellWidth
        s = cellSize

gameAsPicture :: Game -> Picture
gameAsPicture game = pictures [boardAsPicture $ board game, gameTurn game]

boardAsPicture :: Board -> Picture
boardAsPicture board = pictures $ boardAsPicture' board

boardAsPicture' :: Board -> [Picture]
boardAsPicture' [] = []
boardAsPicture' (Void (x,y):cs) = hexagon x y grey : boardAsPicture' cs
boardAsPicture' (Marble c (x,y):cs) = hexagon x y c : boardAsPicture' cs

-- Translate 150.0 300.0 är x- och y-koordinater för textboxen.
gameTurn :: Game -> Picture
gameTurn (Game board (Player c) _) = translate 150.0 300.0 (pictures $ [(scale 0.2 0.2 (text "Player to move:")), (translate 230.0 10.0 (color c (circleSolid 15)))])
