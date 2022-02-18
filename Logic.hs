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
        _ -> onCellClick cell

checkCells :: (Float, Float) -> Game -> Maybe Cell
checkCells = undefined

onCellClick :: Maybe Cell -> Game
onCellClick (Just cell) = undefined

isInCell :: (Float, Float) -> (Float, Float) -> Bool
isInCell (x1, y1) (x2, y2) = sqrt((x1+ abs x2)*(x1+ abs x2) + (y1+ abs y2)*(y1+ abs y2)) < (cellWidth/2)

