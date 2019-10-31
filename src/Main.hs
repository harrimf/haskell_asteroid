module Main where

import Controller
import Model
import View


import Graphics.Gloss

main :: IO ()
main = display FullScreen white (Circle 120)
    
-- main = play (InWindow "Asteroid" (400, 400) (0, 0)) -- Or FullScreen
--               yellow            -- Background color
--               30               -- Frames per second
--               initialState     -- Initial state
--               view             -- View function
--               input            -- Event function
--               step             -- Step function

methode :: Int
methode = test