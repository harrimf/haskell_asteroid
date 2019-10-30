-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
-- step :: Float -> GameState -> IO GameState
-- step secs gstate
--   | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
--   = -- We show a new random number
--     do randomNumber <- randomIO
--        let newNumber = abs randomNumber `mod` 10
--        return $ GameState (ShowANumber newNumber) 0
--   | otherwise
--   = -- Just update the elapsed time
--     return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
-- input :: Event -> GameState -> IO GameState
-- input e gstate = return (inputKey e gstate)

-- inputKey :: Event -> GameState -> GameState
-- inputKey (EventKey (Char c) _ _ _) gstate
--   = -- If the user presses a character key, show that one
--     gstate { infoToShow = ShowAChar c }
-- inputKey _ gstate = gstate -- Otherwise keep the same

inputHandler :: Event -> World -> World
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





--We moeten een grote methode hebben die voor elke frame de