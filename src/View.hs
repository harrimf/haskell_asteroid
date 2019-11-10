{-# LANGUAGE RecordWildCards #-}

module View where

import Graphics.Gloss
import Model

view :: World -> IO Picture
view = return . viewPure

viewPure :: World -> Picture
viewPure world@(World {..}) = pictures ([render player, render projectiles, render asteroids] ++ pausePicture ++ gameOverPicture)
    where
        pausePicture
            | state == Paused = [ translate ((-1)*200) 0 $ scale 0.5 0.5 $ color green (text "Paused")]
            | otherwise = []
        gameOverPicture
            | state == GameOver = [ translate ((-1)*150) 0 $ scale 0.4 0.4 $ color red (text "Game Over")]
            | otherwise = []
