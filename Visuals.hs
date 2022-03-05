module Visuals where

import Game
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

-- HUGO
brighten :: Cell -> Cell
brighten (Void c (x, y)) = Void (bright $ bright grey) (x, y)
brighten (Marble c (x, y)) = Marble (bright c) (x, y)

unbrighten :: Board -> Board
unbrighten [] = []
unbrighten ((Void _ (x, y)) : cs) = Void grey (x, y) : unbrighten cs
unbrighten (c : cs) = c : unbrighten cs

unbrightenGame :: Game -> Game
unbrightenGame game = game {board = unbrighten (board game), player = player game, state = state game}

{- hexagon x y c
    creates a hexagon with center at (x,y) and color c
    x and y are x- and y - coordinates respectively.

    RETURNS: a hexagon with center (x, y) of color c
-}
hexagon :: Float -> Float -> Color -> Picture
hexagon x y c = pictures [color c $ polygon $ hexaCorners x y cellSize, color white $ line $ hexaCorners x y cellSize]

{- hexaCorners x y s
    calculates the corners for a hexagon at (x, y) of size s

    RETURNS: a list of coordinates for the corners of the hexagon at (x, y) of size s

-}
hexaCorners :: Float -> Float -> Float -> Path
hexaCorners x y size =
  [ (x, y - size),
    (x + w / 2, y - size / 2),
    (x + w / 2, y + size / 2),
    (x, y + size),
    (x - w / 2, y + size / 2),
    (x - w / 2, y - size / 2),
    (x, y - size)
  ]
  where
    w = sqrt 3 * size

{- gameAsPicture game
    Renders the current game into a picture.
    Depending on the state of game the picture will contain different elements.
    RETURNS: a picture representation of game
-}
gameAsPicture :: Game -> Picture
gameAsPicture game = case state game of
  StartingScreen -> pictures [pLogoPattern, pSelBs, gameButtons, hintText, rulesText]
  GameOver (Player c) -> pictures [boardAsPicture $ board game, gameOverText c]
  _ -> pictures [scale x x $ boardAsPicture $ board game, gameTurn game]
  where
    x = 1 / fromIntegral (bs game)

{- boardAsPicture board
    Renders the current board into a picture.

    RETURNS: a picture representation of board
-}
boardAsPicture :: Board -> Picture
boardAsPicture board = pictures $ boardAsPicture' board

{- boardAsPicture' board
    helper function for boardAsPicture

-}
boardAsPicture' :: Board -> [Picture]
boardAsPicture' [] = []
boardAsPicture' (Void c (x, y) : cs) = hexagon x y c : boardAsPicture' cs
boardAsPicture' (Marble c (x, y) : cs) = hexagon x y c : boardAsPicture' cs

-- JONATHAN 
{- gameTurn (Player c)
  Draws a picture of the player to move text along with a circle with the color of the current player
  RETURNS: A picture containing "Player to move:" text and a circle of color c
  EXAMPLES: gameTurn Game {player = Player red}   == Translate 85.0 300.0 (Pictures [Scale 0.2 0.2 (Text "Player to move:"),
                                                     Translate 1.0 0.0 (Color grey (Scale 0.2 0.2 (Text "Player to move:"))),
                                                     Translate 230.0 10.0 (Color red (ThickCircle 7.5 15.0))])
            gameTurn Game {player = Player blue}  == Translate 85.0 300.0 (Pictures [Scale 0.2 0.2 (Text "Player to move:"),
                                                     Translate 1.0 0.0 (Color grey (Scale 0.2 0.2 (Text "Player to move:"))),
                                                     Translate 230.0 10.0 (Color blue (ThickCircle 7.5 15.0))])
-}
gameTurn :: Game -> Picture
gameTurn (Game board (Player c) _ _) =
  translate
    85.0
    300.0
    ( pictures $
        [ scale 0.2 0.2 (text "Player to move:"),
          translate 1.5 0 (color grey (scale 0.2 0.2 (text "Player to move:"))),
          translate 230.0 10.0 (color c (circleSolid 15))
        ]
    )

{- gameOverText c
    Draws a Game Over picture showing who won
    RESULTS: A Game Over picture; "Player (Circle) won!", where the circle is matching the victorious player's color c.
    EXAMPLES: gameOverText red == "Player (red circle) won!"
              gameOverText blue == "Player (blue circle) won!"

-}
gameOverText :: Color -> Picture
gameOverText c =
  translate
    50.0
    300.0
    ( pictures $
        [ scale 0.2 0.2 (text $ "Player " ++ show c ++ "won!"),
          translate 230.0 10.0 (color c (circleSolid 15))
        ]
    )


-- List of all buttons used in the main menu/starting screen
testButtons :: [Button]
testButtons =
  [ Button 1 (-230, 0.0),
    Button 2 (-170, 0.0),
    Button 3 (-110, 0.0),
    Button 4 (-50, 0.0),
    Button 5 (10, 0.0),
    Button 6 (70, 0.0),
    Button 7 (130, 0.0),
    Button 8 (190, 0.0)
  ]

{- gameButtons 
    Creates the button pictures and assigns them colors
    RETURNS: A picture of all the buttons in testButtons 
-}
gameButtons :: Picture
gameButtons = pictures $ gameButtons' testButtons colorLst

{- gameButtons' buttonLst colorLst
    Helper function for gameButtons and implementation
    RETURNS: A list of button pictures
-}
gameButtons' :: [Button] -> [Color] -> [Picture]
gameButtons' [] _ = []
gameButtons' ((Button num (x, y)) : xs) (c : cs) = buttons c (x, y) : translate (x - 7.0) (y - 10.0) (scale 0.2 0.2 (text $ show num)) : gameButtons' xs cs

{- buttons c p
    Creates a hexagonal button picture
    RETURNS: A hexagonal picture of color c centered at the coordinates p
-}
buttons :: Color -> Point -> Picture
buttons c (x, y) = pictures [color c $ polygon $ hexaCorners x y 30, color black $ line $ hexaCorners x y 30]


-- List of the color gradient used when making the buttons
colorLst =
  [ c 255 210 210 160,
    c 255 180 180 180,
    c 255 150 150 200,
    c 255 120 120 220,
    c 255 90 90 240,
    c 255 60 60 260,
    c 255 30 30 280,
    c 255 0 0 300
  ]
  where
    c = makeColorI

--- Start logo ---
--OLOF
pLogoPattern = translate 0 230 (pictures $ [pLogo, pBgPattern, pBgPatternR])

pLogo = pictures $ [translate 0 0 pLogoMerge, translate 2 2 pLogoMerge]

pLogoMerge = pictures $ [pLogo1, pLogo2, pLogo3, pLogo4, pLogo5, pLogo6, pLogo7]

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

pSelBs =
  translate
    (-300.0)
    65.0
    ( pictures $
        [ translate 1 0 (color grey (scale 0.2 0.2 (text "Select Board Size:"))),
          (scale 0.2 0.2 (text "Select Board Size:"))
        ]
    )

rulesText =
  translate
    (-300.0)
    (-180.0)
    ( pictures $
        [ translate 1 0 (color grey (scale 0.2 0.2 (text "How to play:"))),
          (scale 0.2 0.2 (text "How to play:")),
          translate 0 (-25) (color grey (scale 0.15 0.15 (text "Clicking on a cell will highlight possible moves."))),
          translate 0 (-50) (color grey (scale 0.15 0.15 (text "To win you must move all your marbles to the opposite"))),
          translate 0 (-75) (color grey (scale 0.15 0.15 (text "triangle on the board.")))
        ]
    )


hintText =
  translate
    (-300.0)
    (-100)
    ( pictures $
        [ translate 1 0 (color grey (scale 0.15 0.15 (text "Hint: You can right-click any time to go back to main menu"))),
          scale 0.15 0.15 (text "Hint: You can right-click any time to go back to main menu")
        ]
    )