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

type Angle      = Float
type FireDelay  = Float -- time between each shot
type Level      = Int 
type Position   = Point
type Score      = Int
type Size       = Vector
type Velocity   = Vector

data AccelerationState = Accelerating | NotAccelerating
  deriving(Eq)
data ShootState = Shooting | NotShooting
  deriving(Eq)
data RotateState = RotatingRight | RotatingLeft | NotRotating
  deriving(Eq)


data State = Playing | Paused | GameOver


data World = World {
    state         :: State,
    level         :: Level,
    score         :: Int,
    screenSize    :: Size,
    entities      :: [Entity]
} 

data Entity
  = SpaceShip {  
        position          :: Position,
        velocity          :: Velocity,
        sprite            :: Picture,
        size              :: Size,
        fireDelay         :: FireDelay,
        --actions
        accelerateAction  :: AccelerationState,
        shootAction       :: ShootState,
        rotateAction      :: RotateState
    }
  | Projectile {  
        position  :: Position,
        velocity  :: Velocity,
        sprite    :: Picture,
        size      :: Size
    } 
  | Asteroid {  
        position  :: Position,
        velocity  :: Velocity,
        sprite    :: Picture,
        size      :: Size 
    } 
  | Bomb {
        position  :: Position,
        velocity  :: Velocity,
        sprite    :: Picture,
        size      :: Size
    }
  | HostileShip {
        position    :: Position,
        velocity    :: Velocity,
        size        :: Size,
        angle       :: Angle,
        sprite      :: Picture,
        fireDelay   :: FireDelay
    } 
  deriving(Eq)

