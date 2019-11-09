{-# LANGUAGE RecordWildCards #-}

module Handlers.FrameHandler(
    frameHandler
) where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Set hiding (map, null)
  
frameHandler :: Float -> World -> IO World
frameHandler seconds world = return (frameHandlerPure seconds world)

frameHandlerPure :: Float -> World -> World
frameHandlerPure seconds world@(World {..})
    | state == Paused = world
    | otherwise = handlePlayerTurn $ handleProjectiles $ handlePlayerFire seconds $ handlePlayerDisplace seconds $ world { time = time + seconds }

handlePlayerDisplace :: Float -> World -> World
handlePlayerDisplace seconds world@(World {..}) 
    | member (SpecialKey KeyUp) keys = world { player    = displace player { playerMoveDuration = playerMoveDuration player + seconds }}
    | otherwise                      = world { player    = displace player { playerMoveDuration = 0 }}

handleProjectiles :: World -> World
handleProjectiles world@(World {..})  | projectiles  /= [] = world { projectiles = displace projectiles }
                                      | otherwise   = world

handlePlayerFire :: Float -> World -> World
handlePlayerFire seconds world@(World {..}) 
    | member (SpecialKey KeySpace) keys && playerFireDelay player > playerFireThreshold player = world {projectiles = fire player world, player = player {playerFireDelay = 0}}
    | otherwise  = world {player = player {playerFireDelay = (playerFireDelay player) + seconds}}

handlePlayerTurn :: World -> World
handlePlayerTurn world@(World {..})     | member (SpecialKey KeyRight) keys && member (SpecialKey KeyLeft) keys  = world
                                        | member (SpecialKey KeyRight) keys = world { player = adjustAngle (SpecialKey KeyRight) player }
                                        | member (SpecialKey KeyLeft) keys  = world { player = adjustAngle (SpecialKey KeyLeft) player }
                                        | otherwise                         = world

adjustAngle :: Key -> Player -> Player
adjustAngle key player@(Player {..}) = player { playerAngle = newAngle }
    where 
        newAngle  | key == (SpecialKey KeyRight) = mod (playerAngle + playerTurnRadius) 360
                  | otherwise                    = mod (playerAngle - playerTurnRadius) 360 
