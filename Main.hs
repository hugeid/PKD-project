module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Game
import Visuals
--https://github.com/tsoding/profun/tree/master/functional/src

window = InWindow "Chinese Checkers" (screenWidth, screenHeight) (100, 100)
initialGame = undefined
gameAsPicture = undefined
transformGame = undefined
backgroundColor = makeColor 255 255 255 255




main :: IO ()
main = display window backgroundColor (pictures makeGrid) --play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)