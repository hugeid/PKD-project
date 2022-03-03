module Game where

import Graphics.Gloss.Data.Color
import Graphics.Gloss

purple :: Color
purple = makeColorI 128 0 128 255
grey :: Color
grey = makeColorI 105 105 105 255
opaqueWhite :: Color
opaqueWhite = makeColorI 255 255 255 128

screenWidth :: Int
screenWidth = 700
screenHeight :: Int
screenHeight = 700

cellSize :: Float
cellSize = fromIntegral screenWidth/8
cellWidth :: Float
cellWidth = sqrt 3 * cellSize
cellHeight :: Float
cellHeight = 2 * cellSize


bSize :: Int
bSize = 2

{- Button represents a button the user can click on
  A Button has an integer value and a tuple containing its coordinates 
-}
data Button = Button Int Point deriving (Show, Eq)

{- Cell represents tile with a color and a point
  Void is an empty tile
  Marble is a player-owned tile, a Marble of color c belongs to the player of color c
-}
data Cell = Void Color Point | Marble Color Point deriving (Eq, Show)

-- Board represents a board of cells
type Board = [Cell]

{- Player represents a player of a color
   A Player of color c can only move cells of the same color.
-}
newtype Player = Player Color deriving (Eq, Show)

{- GameState represents the state of a game
  Running is a running game
  GameOver p is when the game is over and p has won
  ShowingMoves c is when the possible moves from c are highlighted on the board
  StartingScreen is the main menu/splash screen
-}
data GameState = Running | GameOver Player | ShowingMoves Cell | StartingScreen deriving (Eq, Show)

{- Game represents the world of the program
   a Game consists of a board, a player, a state, and a board size.
   in Game {board = b, player = p, state = s, bs = size}
   b is the current board of cells,
   p is the player whose turn it is
   s is the current state of the game
   size is the size of the board

  INVARIANT: (length b) ==  6 * size*size + 6*size + 1
             size >= 1
-}
data Game = Game { board :: Board, player :: Player, state :: GameState, bs :: Int} deriving (Eq, Show)

-- testboard used in various function specifications
testboard :: Board
testboard =
  [ Void grey (0.0, 0.0),
    Void grey (75.77722, 131.25),
    Void grey (151.55444, -0.0),
    Void grey (-75.77722, -131.25),
    Void grey (-151.55444, 0.0),
    Void grey (-75.77722, 131.25),
    Void grey (75.77722, -131.25),
    Marble green (0.0, 262.5),
    Marble red (0.0, -262.5),
    Marble yellow (227.33167, 131.25),
    Marble orange (227.33167, -131.25),
    Marble purple (-227.33167, -131.25),
    Marble blue (-227.33167, 131.25)
  ]

--OLOF
{- Transposes a list of coordinates diagonally. PP means plus Y, plus X (northeast),
  PM means plus Y, minus X (northwest), MP means minus Y, plus X (southeast). MM will not be used.
  The variable m denotes the transposed distance, where e.g. m = 5 results in a shift equal to
  five times the distance between the cell centres of two diagonally adjacent cells.
-}
diagPP lst m = [(x+((cellWidth/2)*m), y+(cellHeight*0.75*m)) | (x,y) <- lst]
diagPM lst m = [(x-((cellWidth/2)*m), y+(cellHeight*0.75*m)) | (x,y) <- lst]
diagMP :: [(Float,Float)] -> Int -> [(Float,Float)]
diagMP lst m = [(x+(cellWidth/2)*(fromIntegral m), y-((cellHeight*0.75)*(fromIntegral m))) | (x,y) <- lst]

{- Transposes a list of coordinates along the x-axis. transpWidthP results in a positive shift,
  whereas transpWidthM results in a negative shift. The variable n indicates the amount of cell widths
 .that are being applied to the transposition.
-}
transpWidthP n lst = [(x+(cellWidth*n),y) | (x,y) <- lst]
transpWidthM n lst = [(x-(cellWidth*n),y) | (x,y) <- lst]

{- Inverts a list of coordinates either vertically, or horizontally.

-}
invertX lst = [(-x,y) | (x,y) <- lst]
invertY lst = [(x,-y) | (x,y) <- lst]

{-Transposes a single pair of coordinates one diagonal step northeast.
-}
addDpoint (x,y) = (x+(cellWidth/2), y+cellHeight*0.75)

{- Creates the basic diagonal line needed for all further operations. For a visual explanation,
  see "D-line" in the attached image "chart.jpg". 
-}
addDline :: Int -> [(Float,Float)] -> [(Float,Float)]
addDline n [] =  addDline (n-1) [addDpoint (0,0)]
addDline 0 lst = lst
addDline n lst = addDline (n-1) (addDpoint (head lst) : lst)

--- Triangles ---

{- Creates triangle 1, which consists of Void cells. See attached image.
EXAMPLES: tri1 6 (addDline 6 [])
-}
tri1 1 lst = lst
tri1 n (x:xs) = (x:xs) ++ tri1 (n-1) (transpWidthP 1 xs)

{- Creates triangle 2, which consists of Void cells. See attached image.
EXAMPLES: tri2 (tri1 6 (addDline 6 []))
-}
tri2 :: [(Float,Float)] -> [(Float,Float)]
tri2 lst = invertY (diagMP lst 1)

{- Creates triangle green, which consists of green cells. See attached image.
EXAMPLES: tricG 6 (tri1 6 (addDline 6 []))
-}
tricG n lst = diagPM lst n

{- Creates triangle red, which consists of red cells. See attached image.
-}
tricR n lst = invertY (tricG n lst)

{- Creates triangle 4 and triangle 5, which both consist of Void cells. See attached image.
EXAMPLES: tri4n5 (tri1 6 (addDline 6 []))
          tri4n5 (tri2 (tri1 6 (addDline 6 [])))
-}
tri4n5 lst = invertY (invertX lst)

{- Creates triangle yellow, which consists of yellow cells. See attached image.
EXAMPLES: tricY 6 (tri2 (tri1 6 (addDline 6 [])))
-}
tricY n lst = diagPP lst n

{- Creates triangle orange, which consists of orange cells. See attached image.
-}
tricO n lst = invertY (tricY n lst)

{- Creates triangle purple, which consists of purple cells. See attached image.
-}
tricP n lst = invertX (tricO n lst)

{- Creates triangle blue, which consists of blue cells. See attached image.
-}
tricB n lst = invertX (tricY n lst)

{- Creates triangle 6, which consists of Void cells. See attached image.
-}
tri6 n lst = transpWidthP n (tricB n lst)

{- Creates triangle 3, which consists of Void cells. See attached image.
-}
tri3 n lst = transpWidthM n (tricO n lst)

--- End of triangles ---

{- Enables pattern matching of colors by associating every color to a string. (Grey is omitted.)
-}
strToColor :: [Char] -> Color
strToColor "cR" = red
strToColor "cO" = orange
strToColor "cP" = purple
strToColor "cY" = yellow
strToColor "cB" = blue
strToColor "cG" = green

{- coordinatesToEncoder
  Turns a list of coordinates and a string into a color encoder.
-}
cTe :: [(Float,Float)] -> String -> [((Float,Float),String)]
cTe lst "grey" = [(z,"") | z <- lst]
cTe lst "yellow" = [(z,"cY") | z <- lst]
cTe lst "orange" = [(z,"cO") | z <- lst]
cTe lst "purple" = [(z,"cP") | z <- lst]
cTe lst "blue" = [(z,"cB") | z <- lst]
cTe lst "green" = [(z,"cG") | z <- lst]
cTe lst "red" = [(z,"cR") | z <- lst]

{- The concatenated list of all color encoders, i.e., a list of coordinates with color strings.
  The list consists of tuples of the form (coordinates, color string).
-}
encodedLst n = cTe ([(0,0)] ++ t1 ++ t2 ++ tri4n5 t1 ++ tri4n5 t2 ++ tri6 f t2 ++ tri3 f t2) "grey" ++ cTe (tricG f t1) "green" ++ cTe (tricR f t1) "red" ++ cTe (tricY f t2) "yellow" ++ cTe (tricO f t2) "orange" ++ cTe (tricP f t2) "purple" ++ cTe (tricB f t2) "blue"
    where
        f = fromIntegral n
        t1 = (tri1 (fromIntegral n) (addDline (fromIntegral n) []))
        t2 = tri2 t1

{- encoderToCell
  Turns a color encoder into a cell.
-}
eTc :: [((Float,Float),String)] -> [Cell]
eTc [((f1,f2),"")] = [Void grey (f1,f2)]
eTc [((f1,f2),c)] = [Marble (strToColor c) (f1,f2)]
eTc (((f1,f2),""):fs) = Void grey (f1,f2) : eTc fs
eTc (((f1,f2),c):fs) = Marble (strToColor c) (f1,f2) : eTc fs

{- Creates a board of size n, where n equals the amount of Marbles at the baseline. This is
  the same amount as the number of cells residing in the side of any of the triangles, as well
  as the number of cells in the D-line (see function addDline).
-}
boardSize n = eTc (encodedLst n)



--- End of improved parametric board ---



initialGame = Game {board = boardSize 1, player = Player red, state = StartingScreen, bs = 1}

winnerBoard :: Int -> Board
winnerBoard n = inverse $ boardSize n 

inverse :: Board -> Board
inverse [] = []
inverse (Marble c (x,y): cs) = Marble c (negate x, negate y):inverse cs
inverse (Void c (x,y): cs) = Marble c (negate x, negate y):inverse cs

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