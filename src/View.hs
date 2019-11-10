{-# LANGUAGE RecordWildCards #-}

module View where

import Graphics.Gloss
import Model

view :: World -> IO Picture
view = return . viewPure

viewPure :: World -> Picture
viewPure world@(World {..}) = pictures ([render player, render projectiles, render asteroids] ++ pausePicture ++ lastTime)
    where
        pausePicture
            | state == Paused = [scale 0.5 0.5 $ translate ((-1)*200) 0 $ color green (text "Paused")] 
            | otherwise = []
        lastTime
            | state == Paused = [ translate ((-1)*150) 100 $ scale 0.1 0.1 $ color yellow $ text lastGameTime]
            | otherwise = []


