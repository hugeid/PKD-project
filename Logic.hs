module Logic where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

import Game
import Visuals


transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game = onClick mousePos game
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

onCellClick (Just (Void (x,y))) game = undefined

showMoves :: Cell -> Game -> Game
showMoves = undefined


replaceCell :: Cell -> Cell -> Board -> Board
replaceCell _ _ [] = []
replaceCell oldCell newCell (c:cs) 
    | oldCell == c = newCell : cs
    | otherwise = c:replaceCell oldCell newCell cs

{-isInCell p1 p2
    checks if p2 is in the cell p1

-}
isInCell :: (Float, Float) -> (Float, Float) -> Bool
isInCell (x1, y1) (x2, y2) = sqrt((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)) < (cellWidth/2)

-- Returns all the neighbouring cells of a certain cell
neighbours :: [(Float, Float)] -> (Float, Float) -> [(Float, Float)]
neighbours listofcoords (x, y) = filter ((==x+w) . fst) samerow ++ filter ((==x-w) . fst) samerow ++ filter ((==x+w/2) . fst) upperrow ++ filter ((==x-w/2) . fst) upperrow ++ filter ((==x+w/2) . fst) lowerrow ++ filter ((==x-w/2) . fst) lowerrow
    where 
        w = cellWidth
        s = cellSize
        samerow = filter ((==y) . snd) listofcoords
        upperrow = filter ((==y + s*1.5) . snd) listofcoords
        lowerrow = filter ((==y - s*1.5) . snd) listofcoords