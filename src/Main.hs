module Main where

import Controller
import Model
import View

import Graphics.Gloss

main :: IO ()
main = display FullScreen white (Circle 80)
-- main = playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
--               black            -- Background color
--               10               -- Frames per second
--               initialState     -- Initial state
--               view             -- View function
--               input            -- Event function
--               step             -- Step function