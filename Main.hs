module Main where

import Graphics.Gloss ( play, makeColor, Display(InWindow) )
import Graphics.Gloss.Data.Color ( makeColor )
import Graphics.Gloss.Interface.Pure.Game
    ( play, makeColor, Display(InWindow) )
import Game ( initialGame, screenHeight, screenWidth )
import Visuals ( gameAsPicture )
import Logic ( transformGame )

window = InWindow "Chinese Checkers" (screenWidth, screenHeight) (100, 100)


backgroundColor = makeColor 255 255 255 255



-- main is run to start the game through play
main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id) 


