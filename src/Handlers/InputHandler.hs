{-# LANGUAGE RecordWildCards #-}

module Handlers.InputHandler(
    inputHandler
) where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Set hiding (map, null)

inputHandler :: Event -> World -> IO World
inputHandler e world@(World {..}) = return (inputHandlerPure e world)

inputHandlerPure :: Event -> World -> World
inputHandlerPure (EventKey (Char 'p') Down _ _) world@(World{..})
    | state == Playing = world {state = Paused}
    | state == Paused = world {state = Playing}
inputHandlerPure (EventKey k Down _ _) world@(World {..}) =  world { keys = insert k keys }
inputHandlerPure (EventKey k Up _ _) world@(World {..}) = world { keys = delete k keys}
inputHandlerPure e world = world 

