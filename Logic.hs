module Logic where

import Debug.Trace
import Game
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Visuals

{- transformGame event game
  handles input events from the user, depending on the game state if the user left-clicks
  GameOver -> the user is sent back to the starting screen.
  StartingScreen -> draws a game with a board size matching the button value the user clicked on.
  otherwise -> depending on the cell the user clicks on, the board is changed.
  independent of game state, right-clicking sends the user back to the starting screen.   

  RETURNS: game updated with the result of event

  EXAMPLES: 
    transformGame (EventKey (MouseButton LeftButton) Up (Modifiers {shift = Up, ctrl = Up, alt = Up}) (-230, 0.0)) (initialGame {state=GameOver (Player red)}) ==
      initialGame 
    transformGame (EventKey (MouseButton LeftButton) Up (Modifiers {shift = Up, ctrl = Up, alt = Up}) (-170, 0.0)) (initialGame {state=StartingScreen}) == 
      initialGame {board = boardSize 2, bs = 2, state=Running}
    transformGame (EventKey (MouseButton LeftButton) Down (Modifiers {shift = Up, ctrl = Up, alt = Up}) (-170, 0.0)) initialGame == initialGame
    transformGame (EventKey (MouseButton RightButton) Up (Modifiers {shift = Up, ctrl = Up, alt = Up}) (-170, 0.0)) initialGame {state = showingMoves} == initialGame
-}
transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos@(x, y)) game =
  case state game of
    GameOver _ -> initialGame
    StartingScreen -> onClick' mousePos game
    _ -> onClick (x * fromIntegral (bs game), y * fromIntegral (bs game)) game
transformGame (EventKey (MouseButton RightButton) Up _ _) _ = initialGame
transformGame _ game = game

--JONATHAN
onClick' :: (Float, Float) -> Game -> Game
onClick' b game =
  let button = checkButtons b game
   in case button of
        Nothing -> game
        _ -> onButtonClick button game

checkButtons :: Point -> Game -> Maybe Button
checkButtons (x, y) game = checkButtons' (x, y) testButtons

checkButtons' :: Point -> [Button] -> Maybe Button
checkButtons' _ [] = Nothing
checkButtons' (x1, y1) ((Button num (x2, y2)) : bs)
  | isInCell (x2, y2) (x1, y1) (sqrt 3 * 30) = Just (Button num (x2, y2))
  | otherwise = checkButtons' (x1, y1) bs

onButtonClick :: Maybe Button -> Game -> Game
onButtonClick (Just (Button num (x, y))) game = newBoardSize num

newBoardSize :: Int -> Game
newBoardSize num = Game {board = boardSize num, player = Player red, state = Running, bs = num}

{- onClick p game
    ...
-}
onClick :: (Float, Float) -> Game -> Game
onClick p game =
  let cell = checkCells p game
   in case cell of
        Nothing -> game
        _ -> onCellClick cell game

{-checkCells p game
    fetches the cell from game that contains the point p

    RETURNS: Just cell where cell is the cell in game's board that contains p,
         or Nothing if p is not in any cell
    EXAMPLES:
      checkCells (0, 5) initialGame = Just (Void grey (0.0,0.0))
      checkCells (500, 5) initialGame = Nothing
-}
checkCells :: (Float, Float) -> Game -> Maybe Cell
checkCells p game = checkCells' p (board game)

{- checkCells' p board
    helper function and implementatin for checkCells'

    RETURNS: Just c where c is the cell in board that contains the point p,
        or Nothing if no such cell is found

    EXAMPLES:
      checkCells' (100, 100) testboard = Just (Void grey (75.77722,131.25))
      checkCells' (250, -580) testboard = Nothing
-}
-- VARIANT: length board
checkCells' :: (Float, Float) -> Board -> Maybe Cell
checkCells' _ [] = Nothing
checkCells' (x1, y1) ((Void c (x2, y2)) : cs)
  | isInCell (x2, y2) (x1, y1) cellWidth = Just (Void c (x2, y2))
  | otherwise = checkCells' (x1, y1) cs
checkCells' (x1, y1) ((Marble c (x2, y2)) : cs)
  | isInCell (x2, y2) (x1, y1) cellWidth = Just (Marble c (x2, y2))
  | otherwise = checkCells' (x1, y1) cs

{- onCellClick c game
    handles what happens when the user has clicked on a cell
    if the color of c is the same as the color of the player whose turn it is,
        the possible moves from that cells are highlighted
    if c is a Void and c is a possible move from c2 (where c2 is the cell from which possible moves are showing),
        a move is made from the c2 to c

    RETURNS: an updated game where a) the possible moves from c2 are showing,
        or b) a move is made from c2 to c,
        or c) nothing is changed

    EXAMPLES:
      onCellClick (Just (Marble red (0.0,-262.5))) initialGame = Game 
        {board = ... same board but the possible moves highlighted ... ,
         player = ... same player sa before ...,
         state = ShowingMoves (Marble red (0.0,-262.5)),
         bs = ... same bs as before ...}

         onCellClick (Just (Void grey (0.0,0.0))) (initialGame {state = Running}) = initialGame
  -}
onCellClick :: Maybe Cell -> Game -> Game
onCellClick (Just (Marble c (x, y))) game =
  trace ("Clicked on " ++ show (x, y)) $
    let Player pc = player game
     in if pc == c
          then showMoves (Marble c (x, y)) game {board = unbrighten (board game)}
          else game
onCellClick (Just (Void c (x, y))) game =
  trace ("Clicked on " ++ show (x, y)) $
    case state game of
      ShowingMoves cell -> move cell (Void c (x, y)) game
      _ -> game

{- move fromCell toCell game
  performs a move from fromCell to toCell and checks if the player who makes the move has won
  RETURNS: an updated game where fromCell and toCell switches coordinates
  PRE: fromCell is a Marble, toCell is a Void
  EXAMPLES: 
    move (Marble red (0.0, -262.5)) (Void grey (75.77722,-131.25)) initialGame == 
      initialGame {
        board = ... (Void grey (0.0, -262.5)),..., (Marble red (75.77722,-131.25)) ...,
        player = same player as before,
        state = same state as before,
        bs = same bs as before
      }
-}
move :: Cell -> Cell -> Game -> Game
move from@(Marble c (x, y)) to@(Void _ (x2, y2)) game
  | isLegalMove from to game =
    let newGame =
          nextTurn
            game
              { board = unbrighten $ replaceCells [to, from] [Marble c (x2, y2), Void grey (x, y)] (board game),
                state = Running
              }
     in if checkWinner (player game) newGame
          then game {board = board newGame, player = player newGame, state = GameOver (player game)}
          else newGame
  | otherwise = game

{-checkWinner p@(Player c) board boardsize
  Checks if a player has won the game.
  all cells with the color c are extracted from the board and then passed on to the helper function checkWinner'.

  RETURNS: True if p has won, otherwise False
  EXAMPLES:
    checkWinner (Player red) initialGame == False
    checkWinner (Player red) initialGame {board = winnerBoard 3, bs = 3} == True
    checkWinner (Player white) initialGame {board = winnerBoard 3, bs = 3} == True
-}
checkWinner :: Player -> Game -> Bool
checkWinner (Player c) game = checkWinner' [cell | cell <- board game, extractColor cell == c] (winnerBoard (bs game))

{- checkWinner' board winBoard
  Helper function from checkWinner
  Checks if all cells in board have a matching cell in winBoard
  
  RETURNS: True if all cells in board have a matching cell in winBoard
    
-}

checkWinner' :: Board -> Board -> Bool
-- VARIANT: length board
checkWinner' [] _ = True
checkWinner' (c : cs) winBoard
  | c `elem` winBoard = checkWinner' cs winBoard
  | otherwise = False

{-showMoves c game
    Displays the possible moves from cell c

    RETURNS: game but with the possible moves from c highlighted in the board of game
-}
showMoves :: Cell -> Game -> Game
showMoves cell game = game {board = replaceCells moves (map brighten moves) (board game), state = ShowingMoves cell}
  where
    moves = legalMoves cell game

{- legalMoves c game
    Calculates the possible moves form cell c in game.
    The legal moves of a cell consists of the Void neighbours of that cell AND the legal jumps from that cell.
    A jump is where the Marble "jumps" over another Marble and lands on the opposite side of that Marble.
    Jumps can be performed multiple times during one move.

    RETURNS: a list of the cells in (board game) to which the c can move
-}
legalMoves :: Cell -> Game -> [Cell]
legalMoves cell game = filter isVoid (neighbours cell (board game)) ++ legalJumps cell game

{-legalJumps c game
    Calculates the legal jumps from c in game.

    RETURNS: a list of cells to which the player can jump to from c
-}
legalJumps :: Cell -> Game -> [Cell]
legalJumps = legalJumpsAux []

{- legalJumpsAux
helper function for

-}
legalJumpsAux :: [Cell] -> Cell -> Game -> [Cell]
legalJumpsAux acc cell game = legalJumps' acc cell (filter (not . isVoid) (neighbours cell (board game))) game

{-legalJumps' acc cell cs game
    helper function from legalJumps'
    cs are the non-Void neighbouring cells to cell
-}
legalJumps' :: [Cell] -> Cell -> [Cell] -> Game -> [Cell]
legalJumps' acc (Marble c1 (x1, y1)) (Marble _ (x2, y2) : cs) game =
  let newCoords = (x1 - 2 * (x1 - x2), y1 - 2 * (y1 - y2))
   in if canMoveTo newCoords (board game)
        then
          let newCell = findCell newCoords (board game)
           in if newCell `notElem` acc
                then legalJumps' (newCell : acc ++ legalJumpsAux (newCell : acc) newCell game) (Marble c1 (x1, y1)) cs game
                else legalJumps' acc (Marble c1 (x1, y1)) cs game
        else legalJumps' acc (Marble c1 (x1, y1)) cs game
legalJumps' acc (Void c1 (x1, y1)) (Marble _ (x2, y2) : cs) game =
  let newCoords = (x1 - 2 * (x1 - x2), y1 - 2 * (y1 - y2))
   in if canMoveTo newCoords (board game)
        then
          let newCell = findCell newCoords (board game)
           in if newCell `notElem` acc
                then legalJumps' (newCell : acc ++ legalJumpsAux (newCell : acc) newCell game) (Void c1 (x1, y1)) cs game
                else legalJumps' acc (Void c1 (x1, y1)) cs game
        else legalJumps' acc (Void c1 (x1, y1)) cs game
legalJumps' acc _ _ _ = acc

{- canMoveTo p board
    Checks if there is a Marble in board that has approximately the same coordinates as p
    RETURNS: True if there is a Marble in board with the coordinates p (without considering decimals)

-}

canMoveTo :: Point -> Board -> Bool
--VARIANT: length board
canMoveTo _ [] = False
canMoveTo p1 (Marble c p2 : cs) = canMoveTo p1 cs
canMoveTo p1 (Void c p2 : cs) = truncateS p1 == truncateS p2 || canMoveTo p1 cs

{- truncateS (x,y)
    Truncates both values in a two-tuple
    RETURNS: a tuple where both x and y are truncated

-}
truncateS :: Integral a => (Float, Float) -> (a, a)
truncateS (x, y) = (truncate x, truncate y)

{- isLegalMove c1 c2 game
    Determines whether the move (c1-->c2) is a legal move in game.

    RETURNS: True if the c1 can move to c2 in game.

-}
isLegalMove :: Cell -> Cell -> Game -> Bool
isLegalMove from to game = elem to $ legalMoves from game

{-replaceCells oldCells newCells board
    Replaces all oldCells with newCells in board

    PRE: length oldCells == length newCells
    RETURNS: board but with each cell in oldCells replaced by a cell in newCells

-}

-- VARIANT: min (length oldCells) (length newCells)
replaceCells :: [Cell] -> [Cell] -> Board -> Board
replaceCells _ [] board = board
replaceCells [] _ board = board
replaceCells (x : xs) (y : ys) board = replaceCells xs ys (replaceCell x y board)

{-replaceCell oldCell newCell board
    replaces oldCell with newCell in board

    RETURNS: board but with oldCell replace by newCell
-}
replaceCell :: Cell -> Cell -> Board -> Board
replaceCell _ _ [] = []
replaceCell oldCell newCell (c : cs)
  | oldCell == c = newCell : cs
  | otherwise = c : replaceCell oldCell newCell cs

{- isVoid c
    RETURNS: True if c is a Void
-}
isVoid :: Cell -> Bool
isVoid (Void _ _) = True
isVoid _ = False


-- JONATHAN 
cyclePlayer :: Player -> Player
cyclePlayer (Player c) = Player $ swapC c

nextTurn :: Game -> Game
nextTurn game = game {player = cyclePlayer $ player game}

swapC :: Color -> Color
swapC c
  | c == red = purple
  | c == purple = blue
  | c == blue = green
  | c == green = yellow
  | c == yellow = orange
  | c == orange = red
  | otherwise = grey

{-isInCell p1 p2
    checks if p2 is in the cell p1

-}
isInCell :: (Float, Float) -> (Float, Float) -> Float -> Bool
isInCell (x1, y1) (x2, y2) w = sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)) < (w / 2)

extractCordslist :: Board -> [Point]
extractCordslist = map extractCords

extractCords :: Cell -> Point
extractCords (Void c (x, y)) = (x, y)
extractCords (Marble c (x, y)) = (x, y)

extractColor :: Cell -> Color
extractColor (Void c (x, y)) = c
extractColor (Marble c (x, y)) = c

listOfNeighbours :: (Float, Float) -> [(Float, Float)]
listOfNeighbours (x, y) =
  [ (x + w, y),
    (x - w, y),
    (x + (w / 2), y + (s * 1.5)),
    (x - (w / 2), y + (s * 1.5)),
    (x + (w / 2), y - (s * 1.5)),
    (x - (w / 2), y - (s * 1.5))
  ]
  where
    s = cellSize
    w = cellWidth

neighbours :: Cell -> Board -> [Cell]
neighbours c = findOnBoard (listOfNeighbours $ extractCords c)

-- neighbours c board = findOnBoard (neighbours' (extractCordslist board) (extractCords c)) board
{-
-- Returns all the neighbouring cells of a certain cell
neighbours' :: [Point] -> (Float, Float) -> [Point]
neighbours' listofcoords (x, y) = filter ((==x+w) . fst) samerow ++ filter ((==x-w) . fst) samerow ++ filter ((==x+w/2) . fst) upperrow ++ filter ((==x-w/2) . fst) upperrow ++ filter ((==x+w/2) . fst) lowerrow ++ filter ((==x-w/2) . fst) lowerrow
    where
        w = cellWidth
        s = cellSize
        samerow = filter ((==y) . snd) listofcoords
        upperrow = filter ((==y + s*1.5) . snd) listofcoords
        lowerrow = filter ((==y - s*1.5) . snd) listofcoords
-}

-- There seems to be something off about the new grid of boardSize where the coordinates doesn't match our original coordinates from listOfNeighbours
-- When clicking on a cell thats on the edge we need a value for the coordinates that doesn't match the coordinates of the board.
findCell :: (Float, Float) -> Board -> Cell
findCell _ [] = Void black (0, 0) -- Temp Cell, removed at findOnBoard
findCell (x, y) (c : cs)
  | let (x2, y2) = extractCords c in (truncate x, truncate y) == (truncate x2, truncate y2) = c
  | otherwise = findCell (x, y) cs

findOnBoard :: [Point] -> Board -> Board
findOnBoard [] _ = []
findOnBoard (x : xs) board = filter (/= Void black (0, 0)) $ findCell x board : findOnBoard xs board