module Handlers.InputHandler(
    inputHandler,
    test
) where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


inputHandler :: Event -> World -> World

test :: Int
test = 5

inputHandler (EventKey (SpecialKey KeyEsc) Down _ _) world = undefined


--Player 1 actions
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) world = undefined

inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) world = undefined

inputHandler (EventKey (SpecialKey KeyRight) Down _ _) world = undefined

inputHandler (EventKey (SpecialKey KeyEnter) Down _ _) world = undefined

--key release actions for p1
inputHandler (EventKey (SpecialKey KeyUp) Up _ _) world = undefined

inputHandler (EventKey (SpecialKey KeyLeft) Up _ _) world = undefined

inputHandler (EventKey (SpecialKey KeyRight) Up _ _) world = undefined


--player 2 actions
inputHandler (EventKey w Down _ _) world = undefined

inputHandler (EventKey a Down _ _) world = undefined

inputHandler (EventKey d Down _ _) world = undefined

inputHandler (EventKey (SpecialKey KeyEnter) Down _ _) w = undefined

--key release actions for p2
inputHandler (EventKey w Up _ _) world = undefined

inputHandler (EventKey a Up _ _) world = undefined

inputHandler (EventKey d Up _ _) world = undefined
