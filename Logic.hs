{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Logic where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Debug.Trace

import Game
import Visuals


transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case state game of
        GameOver _ -> game
        _        -> onClick mousePos game

transformGame (EventKey (MouseButton RightButton) Up _ _) _ = initialGame
transformGame _ game = game

onClick :: (Float, Float) -> Game -> Game
onClick p game = let cell = checkCells p game in
    case cell of
        Nothing -> game
        _       -> onCellClick cell game

checkCells :: (Float, Float) -> Game -> Maybe Cell
checkCells (x, y) game = checkCells' (x, y) (board game)

checkCells' :: (Float, Float) -> Board -> Maybe Cell
checkCells' _ [] = Nothing
checkCells' (x1, y1) ((Void c (x2,y2)):cs)
    | isInCell (x2,y2) (x1,y1) = Just (Void c (x2,y2))
    | otherwise = checkCells' (x1, y1) cs
checkCells' (x1, y1) ((Marble c (x2,y2)):cs)
    | isInCell (x2,y2) (x1,y1) = Just (Marble c (x2,y2))
    | otherwise = checkCells' (x1, y1) cs


onCellClick :: Maybe Cell -> Game -> Game
onCellClick (Just (Marble c (x,y))) game =
    trace ("Clicked on " ++ show (x,y)) $
    let
        Player pc = player game
    in
        if pc == c then
            showMoves (Marble c (x, y)) (unbrightenGame game)
        else
            game

onCellClick (Just (Void c (x,y))) game =
    trace ("Clicked on " ++ show (x,y)) $
    case state game of
    ShowingMoves cell -> move cell (Void c (x,y)) game
    _                 -> game

move :: Cell -> Cell -> Game -> Game
move from@(Marble c (x,y)) to@(Void _ (x2,y2)) game
    | isLegalMove from to game = let newGame = nextTurn Game {board = unbrighten $ replaceCells [to,from] [Marble c (x2,y2),Void grey (x,y)](board game), player = player game, state = Running} 
        in 
            if checkWinner (player game) (board newGame) then
                Game {board = board newGame, player = player newGame, state = GameOver (player game)}
            else
                newGame

    | otherwise = game

checkWinner :: Player -> Board -> Bool
checkWinner (Player c) board = checkWinner' [cell | cell <- board, extractColor cell == c]

checkWinner' :: Board -> Bool
checkWinner' [] = True
checkWinner' (c:cs) 
    | elem c winnerBoard = checkWinner' cs
    | otherwise = False


-- to : Void (grey) (34.641014,-60.0)
-- from : Marble (red) (0.0,-120.0)
{- board : 
[                           Marble green (0.0,120.0),
Marble blue (-103.92304,60.0),Void grey (-34.641014,60.0),Void grey (34.641014,60.0), Marble yellow (103.92304,60.0),
        Void grey (-69.28203,0.0),Void grey (0.0,0.0),Void grey (69.28203,0.0),
Marble purple (-103.92304,-60.0),Void grey (-34.641014,-60.0),Void grey (34.641014,-60.0),Marble orange (103.92304,-60.0),
                        Marble red (0.0,-120.0)]
-}
showMoves :: Cell -> Game -> Game
showMoves cell game = let moves = legalMoves cell game
    in
       Game {board=replaceCells moves (map brighten moves) (board game), player = player game, state = ShowingMoves cell}

legalMoves :: Cell -> Game -> [Cell]
legalMoves cell game = filter isVoid (neighbours cell (board game)) ++ legalJumps cell game

{-legalJumps cell game
    
    RETURNS: a list of cells to which the player can jump to from cell 
-}
legalJumps :: Cell -> Game -> [Cell]
legalJumps cell game = legalJumps' [] cell game

legalJumps' :: [Cell] -> Cell -> Game -> [Cell]
legalJumps' acc cell game = legalJumps'' acc cell  (filter (not . isVoid) (neighbours cell (board game))) game

{-legalJumps'' acc cell cs game
    helper function from legalJumps'
    cs are the non-Void neighbouring cells to cell
-}
legalJumps'' :: [Cell] -> Cell -> [Cell] -> Game -> [Cell]
legalJumps'' acc (Marble c1 (x1,y1)) (Marble _ (x2,y2):cs) game =
    let newCoords = (x1-2*(x1-x2), y1-2*(y1-y2)) in
    if canMoveTo newCoords (board game)
        then let newCell = findCell newCoords (board game) in
            if newCell `notElem` acc
                then
                    legalJumps'' (newCell : acc ++ legalJumps' (newCell:acc) newCell game) (Marble c1 (x1,y1)) cs game
                else
                    legalJumps'' acc (Marble c1 (x1,y1)) cs game
        else legalJumps'' acc (Marble c1 (x1,y1)) cs game

legalJumps'' acc (Void c1 (x1,y1)) (Marble _ (x2,y2):cs) game =
    let newCoords = (x1-2*(x1-x2), y1-2*(y1-y2)) in
    if canMoveTo newCoords (board game)
        then let newCell = findCell newCoords (board game) in
            if newCell `notElem` acc
                then
                    legalJumps'' (newCell : acc ++ legalJumps' (newCell:acc) newCell game) (Void c1 (x1,y1)) cs game
                else
                    legalJumps'' acc (Void c1 (x1,y1)) cs game
        else legalJumps'' acc (Void c1 (x1,y1)) cs game

legalJumps'' acc _ _ _ = acc

canMoveTo :: Point -> Board -> Bool
canMoveTo _ [] = False
canMoveTo p1 (Marble c p2:cs) = canMoveTo p1 cs
canMoveTo p1 (Void c p2:cs) = truncateS p1 == truncateS p2 || canMoveTo p1 cs

truncateS ::Integral a => (Float, Float) -> (a, a)
truncateS (x,y) = (truncate x, truncate y)

isLegalMove :: Cell -> Cell -> Game ->Bool
isLegalMove from to game = elem to $ legalMoves from game

{-replaceCells oldCells newCells board
    replaces all oldCells with newCells in board
-}
replaceCells :: [Cell] -> [Cell] -> Board -> Board
replaceCells _ [] board = board
replaceCells [] _ board = board
replaceCells (x:xs) (y:ys) board = replaceCells xs ys (replaceCell x y board)

{-replaceCell oldCell newCell board
    replaces oldCell with newCell in board
-}
replaceCell :: Cell -> Cell -> Board -> Board
replaceCell _ _ [] = []
replaceCell oldCell newCell (c:cs)
    | oldCell == c = newCell : cs
    | otherwise =  c:replaceCell oldCell newCell cs

isVoid :: Cell -> Bool
isVoid (Void _ _) = True
isVoid _ = False

cyclePlayer :: Player -> Player
cyclePlayer (Player c) = Player $ swapC c

nextTurn :: Game -> Game
nextTurn game = Game {board = board game, player = cyclePlayer $ player game, state = state game}

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
isInCell :: (Float, Float) -> (Float, Float) -> Bool
isInCell (x1, y1) (x2, y2) = sqrt((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)) < (cellWidth/2)

extractCordslist :: Board -> [Point]
extractCordslist xs = map extractCords xs

extractCords :: Cell -> Point
extractCords (Void c (x,y)) = (x, y)
extractCords (Marble c (x,y)) = (x, y)

extractColor :: Cell -> Color
extractColor (Void c (x,y)) = c
extractColor (Marble c (x,y)) = c

listOfNeighbours :: (Float, Float) -> [(Float, Float)]
listOfNeighbours (x,y) = [(x+w,y),(x-w,y),(x+(w/2),y+(s*1.5)),(x-(w/2),y+(s*1.5)),(x+(w/2),y-(s*1.5)),(x-(w/2),y-(s*1.5))]
    where s = cellSize
          w = cellWidth

neighbours :: Cell -> Board -> [Cell]
neighbours c board = findOnBoard (listOfNeighbours $ extractCords c) board


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
findCell _ [] = Void black (0,0) -- Temp Cell, removed at findOnBoard
findCell (x, y) (c:cs)
  | let (x2, y2) = extractCords c in  (truncate x, truncate y) == (truncate x2, truncate y2) = c
  | otherwise = findCell (x, y) cs

findOnBoard :: [Point] -> Board -> Board
findOnBoard [] _ = []
findOnBoard (x:xs) board = filter (/= Void black (0,0)) $ findCell x board : findOnBoard xs board