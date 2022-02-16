module Visuals where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color


import Game

purple = makeColorI 128 0 128 255
grey = makeColorI 105 105 105 255


hexagon :: Float -> Float -> Color -> Picture
hexagon x y c = pictures [color c $ polygon $ hexaCorners x y cellSize, color white $ line $ hexaCorners x y cellSize]


hexaCorners :: Float -> Float -> Float -> [(Float, Float)]
hexaCorners x y size = [(x, y-size), (x + w/2, y-size/2), (x + w/2, y+size/2), (x, y+size), (x - w/2, y+size/2), (x - w/2, y-size/2), (x, y-size)]
    where 
        w = sqrt 3*size


makeGrid = [hexagon 0 0 grey, hexagon (-(w/2)) (-s*1.5) grey, hexagon (w/2) (-s*1.5) grey, hexagon w 0 grey, hexagon (w/2) (s*1.5) grey, hexagon (-(w/2)) (s*1.5) grey, hexagon (-w) 0 grey, hexagon 0 (-s*3) red, hexagon (w*1.5) (-s*1.5) orange, hexagon (w*1.5) (s*1.5) yellow, hexagon 0 (s*3) green, hexagon (-w*1.5) (s*1.5) blue, hexagon (-w*1.5) (-s*1.5) purple]
    where 
        w = cellWidth
        s = cellSize

gameAsPicture :: Game -> Picture
gameAsPicture game = boardAsPicture $ board game

boardAsPicture :: Board -> Picture
boardAsPicture board = undefined
