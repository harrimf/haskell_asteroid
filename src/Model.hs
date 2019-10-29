-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

-- data GameState = GameState {
--                    infoToShow  :: InfoToShow
--                  , elapsedTime :: Float
--                  }

-- initialState :: GameState
-- initialState = GameState ShowNothing 0

type Angle = Float
type FireDelay = Float -- time between each shot
type Level = Int 
type Position = Point
type Score = Int
type Size = Vector
type Velocity = Vector

data AccelerationState = Accelerating | Decelerating | Still
data ShootState = Shooting | NotShooting
data RotateState = RotatingRight | RotatingLeft | NotRotating


data GameState = GameState {  
  state :: State,
  level :: Level,
  world :: World
}

data State = Playing | Paused | GameOver

data World = World {  
  score         :: Int,
  size          :: Size,
  asteroids     :: [Asteroid],
  bombs         :: [Bomb],
  players       :: [Player],
  hostileShips  :: [HostileShip],
  projectiles   :: [Projectile]
}

data SpaceShip = SpaceShip {  
  position  :: Position,
  velocity  :: Velocity,
  size      :: Size
}

data Projectile = Projectile {  
  position  :: Position,
  velocity  :: Velocity,
  size      :: Size
} 

data Asteroid = Asteroid {  
  position  :: Position,
  velocity  :: Velocity,
  size      :: Size
}

data Bomb = Bomb {
  position  :: Position,
  velocity  :: Velocity,
  size      :: Size
 }

data HostileShip = HostileShip {
  position  :: Position,
  velocity  :: Velocity,
  size      :: Size,
  angle     :: Angle,
  fireRate  :: FireDelay
}