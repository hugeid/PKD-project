module Visuals where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Game

brighten :: Cell -> Cell
brighten (Void c (x,y)) = Void (bright $ bright grey) (x,y)
brighten (Marble c (x,y)) = Marble (bright c) (x,y)

unbrighten :: Board -> Board
unbrighten [] = []
unbrighten ((Void _ (x,y)):cs) = Void grey (x,y):unbrighten cs
unbrighten (c:cs) = c:unbrighten cs

unbrightenGame :: Game -> Game
unbrightenGame game = game {board = unbrighten (board game), player = player game, state = state game}

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
gameAsPicture game = case state game of 
    StartingScreen      -> pictures [pLogoPattern, pSelBs, gameButtons]
    GameOver (Player c) -> pictures [boardAsPicture $ board game, gameOverText c]
    _                   -> pictures [scale x x $ boardAsPicture $ board game, gameTurn game]
    where
        x = (1/fromIntegral(bs game))

boardAsPicture :: Board -> Picture
boardAsPicture board = pictures $ boardAsPicture' board

boardAsPicture' :: Board -> [Picture]
boardAsPicture' [] = []
boardAsPicture' (Void c (x,y):cs) = hexagon x y c : boardAsPicture' cs
boardAsPicture' (Marble c (x,y):cs) = hexagon x y c : boardAsPicture' cs

-- Translate 150.0 300.0 är x- och y-koordinater för textboxen.
gameTurn :: Game -> Picture
gameTurn (Game board (Player c) _ _) = translate 85.0 300.0 (pictures $ [(scale 0.2 0.2 (text "Player to move:")), translate 1.5 0 (color grey (scale 0.2 0.2 (text "Player to move:"))), (translate 230.0 10.0 (color c (circleSolid 15)))])

gameOverText :: Color -> Picture
gameOverText c = translate 50.0 300.0 (pictures $ [(scale 0.2 0.2 (text $ "Player " ++ (show c) ++ "won!")), (translate 230.0 10.0 (color c (circleSolid 15)))])

testButtons :: [Button]
testButtons = [Button 1 (-230, 0.0), Button 2 (-170, 0.0), Button 3 (-110, 0.0), Button 4 (-50, 0.0), Button 5 (10, 0.0), Button 6 (70, 0.0), Button 7 (130, 0.0), Button 8 (190, 0.0)]

gameButtons :: Picture
gameButtons = pictures $ gameButtons' testButtons colorLst

gameButtons' :: [Button] -> [Color] -> [Picture]
gameButtons' [] _ = []
gameButtons' ((Button num (x,y)):xs) (c:cs) = buttons c (x,y): translate (x-7.0) (y-10.0) (scale 0.2 0.2 (text $ show num)) : gameButtons' xs cs

buttons :: Color -> Point -> Picture
buttons c (x, y) = pictures [color c $ polygon $ hexaCorners x y 30, color black $ line $ hexaCorners x y 30]

colorLst = [(c 255 210 210 160), (c 255 180 180 180), (c 255 150 150 200), (c 255 120 120 220), (c 255 90 90 240), (c 255 60 60 260), (c 255 30 30 280), (c 255 0 0 300)]
    where
        c = makeColorI

--- Start logo ---

pLogoPattern = translate 0 230 (pictures $ [pLogo, pBgPattern, pBgPatternR])

pLogo = pictures $ [translate 0 0 pLogoMerge, translate 2 2 pLogoMerge]
pLogoMerge = pictures $ [pLogo1,pLogo2,pLogo3,pLogo4,pLogo5,pLogo6,pLogo7]
pLogo1 = translate (-330.0) 0.0 (color red (scale 0.5 0.5 cc))
pLogo2 = translate (-329.0) 0.0 (color orange (scale 0.5 0.5 cc))
pLogo3 = translate (-328.0) 0.0 (color yellow (scale 0.5 0.5 cc))
pLogo4 = translate (-327.0) 0.0 (color green (scale 0.5 0.5 cc))
pLogo5 = translate (-326.0) 0.0 (color blue (scale 0.5 0.5 cc))
pLogo6 = translate (-325.0) 0.0 (color purple (scale 0.5 0.5 cc))
pLogo7 = translate (-324.0) 0.0 (color black (scale 0.5 0.5 cc))
cc = (text "CHINESE CHECKERS")

pBgPattern = translate 0 77 (pictures $ [translate 0 0 pBgPatternMerge2, translate 0 (-10) pBgPatternMerge2alt, translate 0 (-20) pBgPatternMerge2])
pBgPatternR = translate (-10) 52 (rotate 180 pBgPattern)
pBgPatternMerge2 = translate (-30) 0 (pictures $ [translate 0 (0) pBgPattern1, translate 0 (0) pBgPattern2, translate 0 (0) pBgPattern3])
pBgPatternMerge2alt = translate (-30) 0 (pictures $ [translate 0 (0) pBgPattern1alt, translate 0 (0) pBgPattern2alt, translate 0 (0) pBgPattern3alt])
pBgPattern1 = translate (-300.0) (-100.0) (color grey (scale 0.2 0.2 (text "* * * * * * * * * * * * * * * * * * * *")))
pBgPattern2 = translate (-300.0) (-100.0) (color black (scale 0.2 0.2 (text "--------------------------------")))
pBgPattern3 = translate (-300.0) (-100.0) (color red (scale 0.2 0.2 (text "o o o o o o o o o o o o o o o o o o o")))
pBgPattern1alt = translate (-300.0) (-100.0) (color grey (scale 0.2 0.2 (text " * * * * * * * * * * * * * * * * * * * ")))
pBgPattern2alt = translate (-300.0) (-100.0) (color red (scale 0.2 0.2 (text "_______________________________")))
pBgPattern3alt = translate (-300.0) (-100.0) (color red (scale 0.2 0.2 (text " o o o o o o o o o o o o o o o o o o ")))

--- End of Start Logo ---

pSelBs = translate (-300.0) 65.0 (pictures $ [translate 1.5 0 (color grey (scale 0.2 0.2 (text "Select Board Size:"))), (scale 0.2 0.2 (text "Select Board Size:"))])

