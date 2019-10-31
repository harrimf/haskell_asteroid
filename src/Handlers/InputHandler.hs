{-# LANGUAGE RecordWildCards #-}

module Handlers.InputHandler(
    inputHandler
) where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


inputHandler :: Event -> World -> World


inputHandler (EventKey (SpecialKey KeyEsc) Down _ _) world@(World {..}) = undefined
    -- | state == Playing = world {
    --         state = Paused
    --     }
    -- | state == Paused = world {
    --         state = Playing
    --     }

-- data AccelerationState = Accelerating | NotAccelerating
-- deriving(Eq)
-- data ShootState = Shooting | NotShooting
-- deriving(Eq)
-- data RotateState = RotatingRight | RotatingLeft | NotRotating
-- deriving(Eq)


-- data State = Playing | Paused | GameOver


--Player 1 actions
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) world@(World {..}) = 
    world {
        players = (acceleratePlayer Accelerating firstPlayer) : secondPlayer
    }
  where firstPlayer = head players
        secondPlayer = tail players 

inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) world@(World {..}) = 
    world {
        players = (rotatePlayer RotatingLeft firstPlayer) : secondPlayer
    }
    where firstPlayer = head players
          secondPlayer = tail players 

inputHandler (EventKey (SpecialKey KeyRight) Down _ _) world@(World {..}) = 
    world {
        players = (rotatePlayer RotatingRight firstPlayer) : secondPlayer
    }
    where firstPlayer = head players
          secondPlayer = tail players

inputHandler (EventKey (SpecialKey KeyEnter) Down _ _) world@(World {..}) = 
    world {
        players = (shootPlayer Shooting firstPlayer) : secondPlayer
    }
    where firstPlayer = head players
          secondPlayer = tail players
    
--key release actions for p1
inputHandler (EventKey (SpecialKey KeyUp) Up _ _) world@(World {..}) =
    world {
        players = (acceleratePlayer NotAccelerating firstPlayer) : secondPlayer
    }
    where firstPlayer = head players
          secondPlayer = tail players

inputHandler (EventKey (SpecialKey KeyLeft) Up _ _) world@(World {..}) =
    world {
        players = (rotatePlayer NotRotating firstPlayer) : secondPlayer
    }
    where firstPlayer = head players
          secondPlayer = tail players 

inputHandler (EventKey (SpecialKey KeyRight) Up _ _) world@(World {..}) =
    world {
        players = (rotatePlayer NotRotating firstPlayer) : secondPlayer
    }
    where firstPlayer = head players
          secondPlayer = tail players

inputHandler (EventKey (SpecialKey KeyEnter) Up _ _) world@(World {..}) =
    world {
        players = (shootPlayer NotShooting firstPlayer) : secondPlayer
    }
    where firstPlayer = head players
          secondPlayer = tail players
    

--player 2 actions
inputHandler (EventKey w Down _ _) world@(World {..}) =
    world {
        players = firstPlayer : (acceleratePlayer Accelerating (head secondPlayer)) : [] 
    }
    where firstPlayer = head players
          secondPlayer = tail players 

inputHandler (EventKey a Down _ _) world@(World {..}) = 
    world {
        players = firstPlayer : (rotatePlayer RotatingLeft (head secondPlayer)) : []
    }
    where firstPlayer = head players
          secondPlayer = tail players 

inputHandler (EventKey d Down _ _) world@(World {..}) = 
    world {
        players = firstPlayer : (rotatePlayer RotatingRight (head secondPlayer)) : []
    }
    where firstPlayer = head players
          secondPlayer = tail players 

inputHandler (EventKey f Down _ _) world@(World {..}) = 
    world {
        players = firstPlayer : (shootPlayer Shooting (head secondPlayer)) : []
    }
    where firstPlayer = head players
          secondPlayer = tail players 

--key release actions for p2
inputHandler (EventKey w Up _ _) world@(World {..}) = 
    world {
        players = firstPlayer : (acceleratePlayer NotAccelerating (head secondPlayer)) : []
    }
    where firstPlayer = head players
          secondPlayer = tail players

inputHandler (EventKey a Up _ _) world@(World {..}) = 
    world {
        players = firstPlayer : (rotatePlayer NotRotating (head secondPlayer)) : []
    }
    where firstPlayer = head players
          secondPlayer = tail players 

inputHandler (EventKey d Up _ _) world@(World {..}) = 
    world {
        players = firstPlayer : (rotatePlayer NotRotating (head secondPlayer)) : []
    }
    where firstPlayer = head players
          secondPlayer = tail players 

inputHandler (EventKey f Up _ _) world@(World {..}) =
    world {
        players = firstPlayer : (shootPlayer NotShooting (head secondPlayer)) : []
    }
    where firstPlayer = head players
          secondPlayer = tail players

--NOT PRETTY BUT WORKS FOR NOW   
           
-- handle case for twoPlayer mode 