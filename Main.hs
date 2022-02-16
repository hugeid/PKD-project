module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

--https://github.com/tsoding/profun/tree/master/functional/src
screenWidth = 800
screenHeight = 800
window = InWindow "Chinese Checkers" (screenWidth, screenHeight) (100, 100)
initialGame = undefined
gameAsPicture = undefined
transformGame = undefined
backgroundColor = makeColor 255 255 255 255

main :: IO ()
main = display window backgroundColor (Circle 80)--play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)