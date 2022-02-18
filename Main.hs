module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Game
import Visuals
--https://github.com/tsoding/profun/tree/master/functional/src

window = InWindow "Chinese Checkers" (screenWidth, screenHeight) (100, 100)

transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game = game
transformGame _ game = game
backgroundColor = makeColor 255 255 255 255




main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id) 