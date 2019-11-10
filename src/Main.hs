module Main where

import Controller
import Model
import View


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


main :: IO ()
main = do random <- getStdGen --can add custom pause message from file 
          playIO (InWindow "Asteroid" (400,400) (0, 0)) gameColor 30 (initialState random) view inputHandler frameHandler   
    where 
        gameColor = light black