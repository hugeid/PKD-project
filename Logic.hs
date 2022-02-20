module Logic where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

import Game
import Visuals


transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game = 
    case state game of 
        GameOver -> initialGame
        _        -> onClick mousePos game
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
checkCells' (x1, y1) ((Void (x2,y2)):cs) 
    | isInCell (x2,y2) (x1,y1) = Just (Void (x2,y2))
    | otherwise = checkCells' (x1, y1) cs
checkCells' (x1, y1) ((Marble c (x2,y2)):cs) 
    | isInCell (x2,y2) (x1,y1) = Just (Marble c (x2,y2))
    | otherwise = checkCells' (x1, y1) cs


onCellClick :: Maybe Cell -> Game -> Game
onCellClick (Just (Marble c (x,y))) game = let Player pc = player game
    in
        if pc == c then
            showMoves (Marble c (x, y)) game
        else
            game

onCellClick (Just (Void (x,y))) game = case state game of
    ShowingMoves cell -> move cell (Void (x,y)) game
    _            -> game

move :: Cell -> Cell -> Game -> Game
move from to game 
    | isLegalMove from to game = game
    | otherwise = game

showMoves :: Cell -> Game -> Game
showMoves = undefined

legalMoves :: Cell -> Game -> [Cell]
legalMoves cell game = undefined--(filter isVoid . neighbours cell) ++ legalJumps cell game

legalJumps :: Cell -> Game -> [Cell]
legalJumps cell = undefined -- legalJumps' (filter (not . isVoid) . neighbours cell) cell

legalJumps' :: [Cell] -> Cell -> [Cell]
legalJumps' ((Marble c (x1,y1)):cs) = undefined

isLegalMove :: Cell -> Cell -> Game ->Bool
isLegalMove from to game = elem to $ legalMoves from game


replaceCell :: Cell -> Cell -> Board -> Board
replaceCell _ _ [] = []
replaceCell oldCell newCell (c:cs) 
    | oldCell == c = newCell : cs
    | otherwise = c:replaceCell oldCell newCell cs

isVoid :: Cell -> Bool
isVoid (Void _) = True
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
extractCords (Void (x,y)) = (x, y)
extractCords (Marble c (x,y)) = (x, y)

neighbours :: Cell -> Board -> [Cell]
neighbours c board = findOnBoard (neighbours' (extractCordslist board) (extractCords c)) board

-- Returns all the neighbouring cells of a certain cell
neighbours' :: [Point] -> (Float, Float) -> [Point]
neighbours' listofcoords (x, y) = filter ((==x+w) . fst) samerow ++ filter ((==x-w) . fst) samerow ++ filter ((==x+w/2) . fst) upperrow ++ filter ((==x-w/2) . fst) upperrow ++ filter ((==x+w/2) . fst) lowerrow ++ filter ((==x-w/2) . fst) lowerrow
    where 
        w = cellWidth
        s = cellSize
        samerow = filter ((==y) . snd) listofcoords
        upperrow = filter ((==y + s*1.5) . snd) listofcoords
        lowerrow = filter ((==y - s*1.5) . snd) listofcoords

findCell :: Point -> Board -> Cell
findCell _ [] = error "you fucked up"
findCell (x, y) (c:cs)
  | (x, y) == extractCords c = c
  | otherwise = findCell (x, y) cs

findOnBoard :: [Point] -> Board -> Board
findOnBoard [] _ = []
findOnBoard (c:cs) board = findCell c board : findOnBoard cs board