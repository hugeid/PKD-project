module Game where

import Graphics.Gloss
  ( Color,
    Point,
    blue,
    green,
    makeColorI,
    orange,
    red,
    yellow,
  )
import Graphics.Gloss.Data.Color
  ( Color,
    blue,
    green,
    makeColorI,
    orange,
    red,
    yellow,
  )

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
cellSize = fromIntegral screenWidth / 8

cellWidth :: Float
cellWidth = sqrt 3 * cellSize

cellHeight :: Float
cellHeight = 2 * cellSize

{- Button represents a button the user can click on
  A Button has an integer value and a tuple containing its coordinates
  INVARIANT: in Button n p, n >= 1
-}
data Button = Button Int Point deriving (Show, Eq)

{- Cell represents tile with a color and a point
  Void is an empty tile
  Marble is a player-owned tile, a Marble of color c belongs to the player of color c
  INVARIANT: in Void c p, c is either grey or (bright $ bright grey)
             in Marble c p, c is in [red, purple, blue, green, yellow, orange]
-}
data Cell = Void Color Point | Marble Color Point deriving (Eq, Show)

-- Board represents a board of cells
type Board = [Cell]

{- Player represents a player of a color
   A Player of color c can only move cells of color c.
   INVARIANT: in Player c, c must be in [red, purple, blue, green, yellow, orange]
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
   in Game {board = b, player = p, state = s, bs = bSize}
   b is the current board of cells,
   p is the player whose turn it is
   s is the current state of the game
   bSize is the size of the board

  INVARIANT: (length b) ==  6 * bSize² + 6*bSize + 1
             bSize >= 1
-}
data Game = Game {board :: Board, player :: Player, state :: GameState, bs :: Int} deriving (Eq, Show)

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

--- Parametric board ---
-- See chart.jpg for terminology

{- diagPP lst m
    Transposes a list of points diagonally m steps north east. PP means plus Y, plus X.
    RETURNS: a diagonally transposed list of the elements in lst where each element is transposed m steps nort east.
    EXAMPLES: diagPP [] 3 == []
              diagPP [] 0 == []
              diagPP [(1,2)] 0 == [(1.0,2.0)]
              diagPP [(1,2),(3,4)] 5 == [(379.8861,658.25),(381.8861,660.25)]
              diagPP [(1,2)] (-4) == [(-302.1089,-523.0)]
-}
diagPP :: [Point] -> Float -> [Point]
diagPP lst m = [(x + ((cellWidth / 2) * m), y + (cellHeight * 0.75 * m)) | (x, y) <- lst]

{- diagPM lst m
    Transposes a list of points diagonally m steps north west. PM means plus Y, minus X.
    RETURNS: a diagonally transposed list of the elements in lst where each element is transposed m steps nort west.
    EXAMPLES: diagPM [] 3 == []
              diagPM [] 0 == []
              diagPM [(1,2)] 0 == [(1.0,2.0)]
              diagPM [(1,2),(3,4)] 5 == [(-377.8861,658.25),(-375.8861,660.25)]
              diagPM [(1,2)] (-4) == [(304.1089,-523.0)]
-}
diagPM :: [Point] -> Float -> [Point]
diagPM lst m = [(x - ((cellWidth / 2) * m), y + (cellHeight * 0.75 * m)) | (x, y) <- lst]

{- diagMP lst m
    Transposes a list of points diagonally m steps south east. MP means minus Y, plus X.
    RETURNS: a diagonally transposed list of the elements in lst where each element is transposed m steps south east.
    EXAMPLES: diagMP [] 3 == []
              diagMP [] 0 == []
              diagMP [(1,2)] 0 == [(1.0,2.0)]
              diagMP [(1,2),(3,4)] 5 == [(379.8861,-654.25),(381.8861,-652.25)]
              diagMP [(1,2)] (-4) == [(-302.1089,527.0)]
-}
diagMP :: [(Float, Float)] -> Int -> [(Float, Float)]
diagMP lst m = [(x + (cellWidth / 2) * (fromIntegral m), y - ((cellHeight * 0.75) * (fromIntegral m))) | (x, y) <- lst]

{- transpWidthP n lst
    Transposes a list of points along the x-axis. transpWidthP results in a positive shift.
    The variable n indicates the amount of cell widths that are being applied to the transposition.
    RETURNS: a list of each element in lst transposed by n*cellWidth along the x-axis
    EXAMPLES: transpWidthP 0 [] == []
              transpWidthP 3 [] == []
              transpWidthP 0 [(1,2)] == [(1.0,2.0)]
              transpWidthP 5 [(1,2),(3,4)] == [(758.7722,2.0),(760.7722,4.0)]
              transpWidthP (-4) [(1,2)] == [(-605.2178,2.0)]
-}
transpWidthP :: Float -> [(Float, Float)] -> [(Float, Float)]
transpWidthP n lst = [(x + (cellWidth * n), y) | (x, y) <- lst]

{- transpWidthM n lst
    Transposes a list of points along the x-axis. transpWidthM results in a negative shift.
    The variable n indicates the amount of cell widths that are being applied to the transposition.
    RETURNS: a list of each element in lst transposed by (-n*cellWidth) along the x-axis
    EXAMPLES: transpWidthM 0 [] == []
              transpWidthM 3 [] == []
              transpWidthM 0 [(1,2)] == [(1.0,2.0)]
              transpWidthM 5 [(1,2),(3,4)] == [(-756.7722,2.0),(-754.7722,4.0)]
              transpWidthM (-4) [(1,2)] == [(607.2178,2.0)]
-}
transpWidthM :: Float -> [(Float, Float)] -> [(Float, Float)]
transpWidthM n lst = [(x - (cellWidth * n), y) | (x, y) <- lst]

{- invertX lst
    Inverts a list of points horizontally (inverts the x-axis).
    RETURNS: a list of the elements in lst that has been inverted along the x-axis
    EXAMPLES: invertX [] == []
              invertX [(1,2)] == [(-1.0,2.0)]
              invertX [(1,2),(3,4)] == [(-1.0,2.0),(-3.0,4.0)]
              invertX [((-1),2),(3,(-4))] == [(1.0,2.0),(-3.0,-4.0)]
-}
invertX :: [(Float, Float)] -> [(Float, Float)]
invertX lst = [(-x, y) | (x, y) <- lst]

{- invertY lst
    Inverts a list of points vertically (inverts the y-axis).
    RETURNS: a list of the elements in lst that has been inverted along the y-axis
    EXAMPLES: invertY [] == []
              invertY [(1,2)] == [(1.0,-2.0)]
              invertY [(1,2),(3,4)] == [(1.0,-2.0),(3.0,-4.0)]
              invertY [((-1),2),(3,(-4))] == [(-1.0,-2.0),(3.0,4.0)]
-}
invertY :: [(Float, Float)] -> [(Float, Float)]
invertY lst = [(x, -y) | (x, y) <- lst]

{- addDpoint point
    Transposes a single pair of coordinates one diagonal step north east.
    RETURNS: point transposed one diagonal step north east.
    EXAMPLES: addDpoint (0,0) == (75.77722,131.25)
              addDpoint (1,1) == (76.77722,132.25)
              addDpoint ((-1),(-1)) == (74.77722,130.25)
-}
addDpoint :: (Float, Float) -> (Float, Float)
addDpoint (x, y) = (x + (cellWidth / 2), y + cellHeight * 0.75)

{- addDline n lst
    Creates the basic diagonal line of length n (D-line), which is needed for
    all further operations. In order to generate a properly shaped board,
    this function always takes an empty list as argument. However, this
    is not a precondition. n should correspond to that of boardSize.
    PRE: n >= 0
    RETURNS: a list of length (n+length lst) making up a diagonal line
    EXAMPLES: addDline 0 [] == []
              addDline 5 [] == [(378.8861,656.25),(303.1089,525.0),(227.33167,393.75),(151.55444,262.5),(75.77722,131.25)]
              addDline 2 [(1,2)] == [(152.55444,264.5),(76.77722,133.25),(1.0,2.0)]
              addDline 0 [(1,2),(3,4)] == [(1.0,2.0),(3.0,4.0)]
              addDline 2 [(1,2),(3,4)] == [(152.55444,264.5),(76.77722,133.25),(1.0,2.0),(3.0,4.0)]
-}

addDline :: Int -> [(Float,Float)] -> [(Float,Float)]
-- VARIANT: n
addDline 0 lst = lst
addDline n [] = addDline (n - 1) [addDpoint (0, 0)]
addDline n lst = addDline (n - 1) (addDpoint (head lst) : lst)

--- Triangles ---

{- tri1 n lst
    Creates triangle 1, which consists of Void cells. In order to generate a properly shaped board,
    this function always takes a D-line-list as argument, and n should correspond to that of boardSize.
    PRE: n > 0. length lst >= n-1
    RETURNS: a list of points making up an equilateral triangle in which each edge has n points
    EXAMPLES: tri1 1 (addDline 1 []) == [(75.77722,131.25)]
              tri1 2 (addDline 2 []) == [(151.55444,262.5),(75.77722,131.25),(227.33167,131.25)]
              tri1 1 [] == []
              tri1 1 [(1,2)] == [(1.0,2.0)]
              tri1 2 [(1,2)] == [(1.0,2.0)]
              tri1 3 [(1,2),(2,3)] == [(1.0,2.0),(2.0,3.0),(153.55444,3.0)]
-}
tri1 :: Int -> [(Float, Float)] -> [(Float, Float)]
-- VARIANT: n-1
tri1 1 lst = lst
tri1 n (x : xs) = (x : xs) ++ tri1 (n - 1) (transpWidthP 1 xs)

{- tri2 lst
    Creates triangle 2, which consists of Void cells. In order to generate a properly shaped board,
    this function always takes a triangle 1-list as argument.
    RETURNS: a y-axis inversion of the triangle formed by the points in lst, transposed one step south east
    EXAMPLES: tri2 (tri1 1 (addDline 1 [])) == [(151.55444,-0.0)]
              tri2 (tri1 2 (addDline 2 [])) == [(227.33167,-131.25),(151.55444,-0.0),(303.1089,-0.0)]
              tri2 [] == []
              tri2 [(1,2)] == [(76.77722,129.25)]
              tri2 [(1,2),(2,3)] == [(76.77722,129.25),(77.77722,128.25)]
-}
tri2 :: [(Float, Float)] -> [(Float, Float)]
tri2 lst = invertY (diagMP lst 1)

{- tricG n lst
    Creates triangle green, which consists of green cells. In order to generate a properly shaped board,
    this function always takes a triangle 1-list as argument, and n should correspond to that of boardSize.
    RETURNS: list of points making up a triangle
    EXAMPLES: tricG 1 (tri1 1 (addDline 1 [])) == [(0.0,262.5)]
              tricG 2 (tri1 2 (addDline 2 [])) == [(0.0,525.0),(-75.77722,393.75),(75.77722,393.75)]
              tricG 0 [] == []
              tricG 3 [] == []
              tricG 3 [(1,2)] == [(-226.33167,395.75)]
              tricG (-3) [(1,2),(0,0),(-1,-2)] == [(228.33167,-391.75),(227.33167,-393.75),(226.33167,-395.75)]
-}
tricG :: Float -> [(Float, Float)] -> [(Float, Float)]
tricG n lst = diagPM lst n

{- tricR n lst
    Creates triangle red, which consists of red cells. In order to generate a properly shaped board,
    this function always takes a triangle 1-list as argument, and n should correspond to that of boardSize.
    RETURNS: list of points making up a triangle
    EXAMPLES: tricR 1 (tri1 1 (addDline 1 [])) == [(0.0,-262.5)]
              tricR 2 (tri1 2 (addDline 2 [])) == [(0.0,-525.0),(-75.77722,-393.75),(75.77722,-393.75)]
              tricR 0 [] == []
              tricR 3 [] == []
              tricR 3 [(1,2)] == [(-226.33167,-395.75)]
              tricR (-3) [(1,2),(0,0),(-1,-2)] == [(228.33167,391.75),(227.33167,393.75),(226.33167,395.75)]
-}
tricR :: Float -> [(Float, Float)] -> [(Float, Float)]
tricR n lst = invertY (tricG n lst)

{- tri4n5 lst
    Creates triangle 4 and triangle 5, which both consist of Void cells. In order to generate a properly shaped board,
    this function either takes a triangle 1-list, or a triangle 2-list, as argument.
    RETURNS: list of points making up a triangle
    EXAMPLES: tri4n5 (tri1 1 (addDline 1 [])) == [(-75.77722,-131.25)]
              tri4n5 (tri1 2 (addDline 2 [])) == [(-151.55444,-262.5),(-75.77722,-131.25),(-227.33167,-131.25)]
              tri4n5 (tri2 (tri1 1 (addDline 1 []))) == [(-151.55444,0.0)]
              tri4n5 (tri2 (tri1 2 (addDline 2 []))) == [(-227.33167,131.25),(-151.55444,0.0),(-303.1089,0.0)]
              tri4n5 [] == []
              tri4n5 [(1,2)] == [(-1.0,-2.0)]
              tri4n5 [(1,2),(-1,-2)] == [(-1.0,-2.0),(1.0,2.0)]
-}
tri4n5 :: [(Float, Float)] -> [(Float, Float)]
tri4n5 lst = invertY (invertX lst)

{- tricY n lst
    Creates triangle yellow, which consists of yellow cells. In order to generate a properly shaped board,
    this function always takes a triangle 2-list as argument, and n should correspond to that of boardSize.
    RETURNS: list of points making up a triangle
    EXAMPLES: tricY 1 (tri2 (tri1 1 (addDline 1 []))) == [(227.33167,131.25)]
              tricY 2 (tri2 (tri1 2 (addDline 2 []))) == [(378.8861,131.25),(303.1089,262.5),(454.66333,262.5)]
              tricY 0 [] == []
              tricY 3 [] == []
              tricY 3 [(1,2)] == [(228.33167,395.75)]
              tricY (-3) [(1,2),(0,0),(-1,-2)] == [(-226.33167,-391.75),(-227.33167,-393.75),(-228.33167,-395.75)]
-}
tricY :: Float -> [(Float, Float)] -> [(Float, Float)]
tricY n lst = diagPP lst n

{- tricO n lst
    Creates triangle orange, which consists of orange cells. In order to generate a properly shaped board,
    this function always takes a triangle 2-list as argument, and n should correspond to that of boardSize.
    RETURNS: list of points making up a triangle
    EXAMPLES: tricO 1 (tri2 (tri1 1 (addDline 1 []))) == [(227.33167,-131.25)]
              tricO 2 (tri2 (tri1 2 (addDline 2 []))) == [(378.8861,-131.25),(303.1089,-262.5),(454.66333,-262.5)]
              tricO 0 [] == []
              tricO 3 [] == []
              tricO 3 [(1,2)] == [(228.33167,-395.75)]
              tricO (-3) [(1,2),(0,0),(-1,-2)] == [(-226.33167,391.75),(-227.33167,393.75),(-228.33167,395.75)]
-}
tricO :: Float -> [(Float, Float)] -> [(Float, Float)]
tricO n lst = invertY (tricY n lst)

{- tricP n lst
    Creates triangle purple, which consists of purple cells. In order to generate a properly shaped board,
    this function always takes a triangle 2-list as argument, and n should correspond to that of boardSize.
    RETURNS: list of points making up a triangle
    EXAMPLES: tricP 1 (tri2 (tri1 1 (addDline 1 []))) == [(-227.33167,-131.25)]
              tricP 2 (tri2 (tri1 2 (addDline 2 []))) == [(-378.8861,-131.25),(-303.1089,-262.5),(-454.66333,-262.5)]
              tricP 0 [] == []
              tricP 3 [] == []
              tricP 3 [(1,2)] == [(-228.33167,-395.75)]
              tricP (-3) [(1,2),(0,0),(-1,-2)] == [(226.33167,391.75),(227.33167,393.75),(228.33167,395.75)]
-}
tricP :: Float -> [(Float, Float)] -> [(Float, Float)]
tricP n lst = invertX (tricO n lst)

{- tricB n lst
    Creates triangle blue, which consists of blue cells. In order to generate a properly shaped board,
    this function always takes a triangle 2-list as argument, and n should correspond to that of boardSize.
    RETURNS: list of points making up a triangle
    EXAMPLES: tricB 1 (tri2 (tri1 1 (addDline 1 []))) == [(-227.33167,131.25)]
              tricB 2 (tri2 (tri1 2 (addDline 2 []))) == [(-378.8861,131.25),(-303.1089,262.5),(-454.66333,262.5)]
              tricB 0 [] == []
              tricB 3 [] == []
              tricB 3 [(1,2)] == [(-228.33167,395.75)]
              tricB (-3) [(1,2),(0,0),(-1,-2)] == [(226.33167,-391.75),(227.33167,-393.75),(228.33167,-395.75)]
-}
tricB :: Float -> [(Float, Float)] -> [(Float, Float)]
tricB n lst = invertX (tricY n lst)

{- tri6 n lst
    Creates triangle 6, which consists of Void cells. In order to generate a properly shaped board,
    this function always takes a triangle 2-list as argument, and n should correspond to that of boardSize.
    RETURNS: list of points making up a triangle
    EXAMPLES: tri6 1 (tri2 (tri1 1 (addDline 1 []))) == [(-75.77722,131.25)]
              tri6 2 (tri2 (tri1 2 (addDline 2 []))) == [(-75.77722,131.25),(0.0,262.5),(-151.55444,262.5)]
              tri6 0 [] == []
              tri6 3 [] == []
              tri6 3 [(1,2)] == [(226.33167,395.75)]
              tri6 (-3) [(1,2),(0,0),(-1,-2)] == [(-228.33167,-391.75),(-227.33167,-393.75),(-226.33167,-395.75)]
-}
tri6 :: Float -> [(Float, Float)] -> [(Float, Float)]
tri6 n lst = transpWidthP n (tricB n lst)

{- Creates triangle 3, which consists of Void cells. See chart.jpg. In order to generate a properly shaped board,
    this function always takes a triangle 2-list as argument, and n should correspond to that of boardSize.
    RETURNS: list of points making up a triangle
    EXAMPLES: tri3 1 (tri2 (tri1 1 (addDline 1 []))) == [(75.77722,-131.25)]
              tri3 2 (tri2 (tri1 2 (addDline 2 []))) == [(75.77722,-131.25),(0.0,-262.5),(151.55444,-262.5)]
              tri3 0 [] == []
              tri3 3 [] == []
              tri3 3 [(1,2)] == [(-226.33167,-395.75)]
              tri3 (-3) [(1,2),(0,0),(-1,-2)] == [(228.33167,391.75),(227.33167,393.75),(226.33167,395.75)]
-}
tri3 :: Float -> [(Float, Float)] -> [(Float, Float)]
tri3 n lst = transpWidthM n (tricO n lst)

--- End of triangles ---

{- strToColor str
    Enables pattern matching of colors by associating every color to a string. (Grey is omitted.)
    PRE: String must match exactly one of the six patterns.
    RETURNS: the color matching str
    EXAMPLES: strToColor "cR" == red
-}
strToColor :: [Char] -> Color
strToColor "cR" = red
strToColor "cO" = orange
strToColor "cP" = purple
strToColor "cY" = yellow
strToColor "cB" = blue
strToColor "cG" = green

{- cTe lst str
    cTe (coordinates to encoder) turns a list of points and a string into a color encoder.
    PRE: String must match exactly one of the seven patterns.
    RETURNS: a list of the elements in lst encoded with str
    EXAMPLES: cTe [(1,2)] "red" == [((1.0,2.0),"cR")]
              cTe [(1,2)] "grey" == [((1.0,2.0),"")]
              cTe [] "red" == []
-}
cTe :: [Point] -> String -> [(Point, String)]
cTe lst "grey" = [(z, "") | z <- lst]
cTe lst "yellow" = [(z, "cY") | z <- lst]
cTe lst "orange" = [(z, "cO") | z <- lst]
cTe lst "purple" = [(z, "cP") | z <- lst]
cTe lst "blue" = [(z, "cB") | z <- lst]
cTe lst "green" = [(z, "cG") | z <- lst]
cTe lst "red" = [(z, "cR") | z <- lst]

{- encodedLst n
    Renders the concatenated list of all color encoders, i.e., a list of points with
    color strings, for a board of size n.
    PRE: n > 0.
    RETURNS: a list of color encoded points making up a board of size n
    EXAMPLES: encodedLst 1 == [((0.0,0.0),""),((75.77722,131.25),""),((151.55444,-0.0),""),
    ((-75.77722,-131.25),""),((-151.55444,0.0),""),((-75.77722,131.25),""),((75.77722,-131.25),""),
    ((0.0,262.5),"cG"),((0.0,-262.5),"cR"),((227.33167,131.25),"cY"),((227.33167,-131.25),"cO"),
    ((-227.33167,-131.25),"cP"),((-227.33167,131.25),"cB")]
-}
encodedLst :: Int -> [((Float, Float), String)]
encodedLst n =
  cTe
    ([(0, 0)] ++ t1 ++ t2 ++ tri4n5 t1 ++ tri4n5 t2 ++ tri6 f t2 ++ tri3 f t2) "grey"
    ++ cTe (tricG f t1) "green"
    ++ cTe (tricR f t1) "red"
    ++ cTe (tricY f t2) "yellow"
    ++ cTe (tricO f t2) "orange"
    ++ cTe (tricP f t2) "purple"
    ++ cTe (tricB f t2) "blue"
  where
    f = fromIntegral n
    t1 = tri1 (fromIntegral n) (addDline (fromIntegral n) [])
    t2 = tri2 t1

{- eTc lst
    eTc (encoder to cell) turns a list of color encoders into a board.
    PRE: Must be a valid color encoder, i.e., string in [((Float,Float),String)]
    must match the output of cTe. List cannot be empty.
    RETURNS: a board of cells created from the elements in lst
    EXAMPLES: eTc [((1,2),"cR")] == [Marble red (1.0,2.0)]
              eTc [((1,2),"cR"), ((3,4),"cB")] ==
    [Marble red (1.0,2.0),Marble blue (3.0,4.0)]
              eTc [((-1,-2),"")] == [Void grey (-1.0,-2.0)]
-}
eTc :: [((Float, Float), String)] -> Board
-- VARIANT: length of lst.
eTc [((f1, f2), "")] = [Void grey (f1, f2)]
eTc [((f1, f2), c)] = [Marble (strToColor c) (f1, f2)]
eTc (((f1, f2), "") : fs) = Void grey (f1, f2) : eTc fs
eTc (((f1, f2), c) : fs) = Marble (strToColor c) (f1, f2) : eTc fs

{- boardSize n
    Creates a board of size n, where n equals the amount of Marbles at the baseline. This is
    the same amount as the number of cells residing in the side of any of the triangles, as well
    as the number of cells in the D-line (see function addDline). The total number of cells
    equals 6*(n^2) + 6*n + 1.
    PRE: n > 0.
    RETURNS: a board of size n
    EXAMPLES: boardSize 1 == [Void grey (0.0,0.0),
    Void grey (75.77722,131.25),
    Void grey (151.55444,-0.0),
    Void grey (-75.77722,-131.25),
    Void grey (-151.55444,0.0),
    Void grey (-75.77722,131.25),
    Void grey (75.77722,-131.25),
    Marble green (0.0,262.5),Marble red (0.0,-262.5),
    Marble yellow (227.33167,131.25),Marble orange (227.33167,-131.25),
    Marble purple (-227.33167,-131.25),
    Marble blue (-227.33167,131.25)]
-}
boardSize :: Int -> Board
boardSize n = eTc (encodedLst n)

--- End of parametric board ---

-- Creates the initial game that is used in the main function of Main.hs.
initialGame :: Game
initialGame = Game {board = boardSize 1, player = Player red, state = StartingScreen, bs = 1}

{- winnerBoard n
    Creates an inverted starting board, which is used as a blueprint for the winner's board.
    PRE: n > 0.
    RETURNS: a board of size n where all marbles are in their expected winning position
    EXAMPLES: winnerBoard 1 ==
                              [ Marble grey (-0.0, -0.0),
                                  Marble grey (-75.77722, -131.25),
                                  Marble grey (-151.55444, 0.0),
                                  Marble grey (75.77722, 131.25),
                                  Marble grey (151.55444, -0.0),
                                  Marble grey (75.77722, -131.25),
                                  Marble grey (-75.77722, 131.25),
                                  Marble green (-0.0, -262.5),
                                  Marble red (-0.0, 262.5),
                                  Marble yellow (-227.33167, -131.25),
                                  Marble orange (-227.33167, 131.25),
                                  Marble purple (227.33167, 131.25),
                                  Marble blue (227.33167, -131.25)
                              ]
-}

winnerBoard :: Int -> Board
winnerBoard n = inverse $ boardSize n

{- inverse b
    Inverts a board.
    RETURNS: a board of the cells in b where each cell's coordinates are inverted.
    EXAMPLES: inverse [Void grey (0.0, 0.0)] ==
    [Void grey (-0.0,-0.0)]
              inverse [Marble red (0.0, -262.5), Marble blue (-227.33167, 131.25)] ==
    [Marble red (-0.0,262.5),Marble blue (227.33167,-131.25)]
              inverse [] == []
-}
inverse :: Board -> Board
-- VARIANT: length of b.
inverse [] = []
inverse (Marble c (x, y) : cs) = Marble c (negate x, negate y) : inverse cs
inverse (Void c (x, y) : cs) = Void c (negate x, negate y) : inverse cs

{- Model of a board of size 1.

       g
b    0   0    y
  0    0    0
p    0   0    o
       r

-}
