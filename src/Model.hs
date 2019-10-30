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
type Duration   = Int

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
    players       :: [Player],
    projectiles   :: [Projectile],
    asteroids     :: [Asteroid],
    bombs         :: [Bomb],
    hostileShips  :: [HostileShip]
} 

data Player = Player {  
        playerPosition          :: Position,
        playerVelocity          :: Velocity,
        playerSprite            :: Picture,
        playerSize              :: Size,
        playerFireDelay         :: FireDelay,
        --actions
        accelerateAction        :: AccelerationState,
        shootAction             :: ShootState,
        rotateAction            :: RotateState
    } deriving(Eq)

data Projectile = Projectile {  
        projectilePosition            :: Position,
        projectileVelocity            :: Velocity,
        projectileSprite              :: Picture,
        projectileDuration            :: Duration,
        projectileSize                :: Size
    }  deriving(Eq)

data Asteroid = Asteroid {  
        asteroidPosition          :: Position,
        asteroidVelocity          :: Velocity,
        asteroidSprite            :: Picture,
        asteroidSize              :: Size 
    } deriving(Eq)
data Bomb = Bomb {
        bombPosition      :: Position,
        bombVelocity      :: Velocity,
        bombSprite        :: Picture,
        bombSize          :: Size
    } deriving(Eq)
  
data HostileShip = HostileShip {
        hostilePosition    :: Position,
        hostileVelocity    :: Velocity,
        hostileSize        :: Size,
        hostileAngle       :: Angle,
        hostileSprite      :: Picture,
        hostileFireDelay   :: FireDelay
    } deriving(Eq)

