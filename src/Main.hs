module Main where

import Controller
import Model
import View


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


main :: IO ()
main = playIO (InWindow "Asteroid" (400,400) (0, 0)) 
              asteroidColor           
              30               
              initialState     
              view             
              inputHandler    
              frameHandler   
    where 
        asteroidColor = light black