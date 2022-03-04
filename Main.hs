module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Game
import Visuals
import Logic

window = InWindow "Chinese Checkers" (screenWidth, screenHeight) (100, 100)


backgroundColor = makeColor 255 255 255 255




main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id) 


