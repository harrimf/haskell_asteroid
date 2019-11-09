{-# LANGUAGE RecordWildCards #-}

module Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Set hiding (map)


-- data Asteroid = Asteroid {
--     asteroidAngle             :: Angle,
--     asteroidDeleted           :: Deleted,  
--     asteroidPosition          :: Position,
--     asteroidVelocity          :: Velocity,
--     asteroidSprite            :: Picture,
--     asteroidSize              :: Size 
-- } deriving(Eq)

-- data Bomb = Bomb {
--     bombAngle         :: Angle,
--     bombDeleted       :: Deleted,
--     bombPosition      :: Position,
--     bombVelocity      :: Velocity,
--     bombSprite        :: Picture,
--     bombSize          :: Size
-- } deriving(Eq)


type Angle      = Int
type Deleted    = Bool
type FireDelay  = Float -- time between each shot
type Position   = Point
type Score      = Int
type Size       = Vector
type Time       = Float
type Acceleration = Float
type Scale      = Float
type Speed      = Float

-- data AccelerationState = Accelerating | NotAccelerating
--   deriving(Eq)
-- data ShootState = Shooting | NotShooting
--   deriving(Eq)
-- data RotateState = RotatingRight | RotatingLeft | NotRotating
--   deriving(Eq)


data State = Playing | Paused | GameOver
  deriving(Eq)


data World = World {
    state         :: State,
    score         :: Score,
    time          :: Time, 
    screenSize    :: Size,
    randomGen     :: StdGen,
    keys          :: Set Key,
    player        :: Player,
    projectiles   :: [Projectile],
    asteroids     :: [Asteroid]
    -- bombs         :: [Bomb]
    --hostileShips  :: [HostileShip]
} 

data Player = Player {  
    playerAngle             :: Angle,
    playerDeleted           :: Deleted, 
    playerPosition          :: Position,
    playerSpeed             :: Speed,
    playerAcceleration      :: Acceleration,
    playerSprite            :: Picture,
    playerFireDelay         :: FireDelay,
    playerFireThreshold     :: FireDelay,
    playerDefaultSpeed      :: Speed,
    playerMaxSpeed          :: Speed,
    playerScale             :: Scale,
    playerMoveDuration      :: Time,
    playerTurnRadius        :: Angle
   
} deriving(Eq)

data Projectile = Projectile {  
    projectileAngle               :: Angle,
    projectileDeleted             :: Deleted,
    projectilePosition            :: Position,
    projectileSpeed               :: Speed,
    projectileSprite              :: Picture,
    projectileScale               :: Scale
}  deriving(Eq)


data Asteroid = Asteroid {
    asteroidAngle             :: Angle,
    asteroidDeleted           :: Deleted,  
    asteroidPosition          :: Position,
    asteroidSpeed             :: Speed,
    asteroidSprite            :: Picture,
    asteroidScale             :: Scale 
} deriving(Eq)


--Rendering class 
class Render a where 
  render :: a -> Picture

instance (Render a) => Render [a] where
  render a = pictures (map render a)

instance Render Player where
  render player@(Player {..}) = color red $ polygon [c1,c2,c3]
    where 
      (x,y) = playerPosition
      c1 = renderCoordinate playerAngle (x,y) (playerScale,playerScale) 
      c2 = renderCoordinate playerAngle (x,y) (0,(-1)*playerScale)
      c3 = renderCoordinate playerAngle (x,y) ((-1)*playerScale,playerScale)

instance Render Projectile where
  render projectile@(Projectile {..}) = uncurry translate projectilePosition $ projectileSprite

-- Class for handling player firing projectiles
class Fire a where
  fire :: a -> World -> [Projectile]
  
instance Fire Player where
  fire player@(Player {..}) world = newProjectile : projectiles world
    where 
      newProjectile = adjustProjectile initialProjectile (playerAngle) (playerPosition)

-- Class to handle movement
class Displace a where
  displace :: a -> a

instance (Displace a) => Displace [a] where
  displace a = map displace a

instance Displace Player where
  displace player@(Player {..})  = player { playerPosition    = newPosition }
    where 
          newPosition | x2 > xBound || x2 < (-1) * xBound = (-1*x2, y2)
                      | y2 > yBound || y2 < (-1) * yBound = (x2, -1*y2)
                      | otherwise                         = (x2, y2)
          (x,y) = playerPosition
          (dx, dy) = generateDelta player
          (x2, y2) = (x+dx, y+dy)
          (w, h) = screenSize initialState
          (xBound, yBound) = (((w / 2) + playerScale), ((h / 2) + playerScale))

instance Displace Projectile where
  displace projectile@(Projectile {..}) = projectile { projectilePosition = (x+dx, y+dy) }
    where 
      (x,y) = projectilePosition
      (dx, dy) = generateDelta projectile

--Class for handling position changes in regard to acceleration & speed changes
class PositionDelta a where
  generateDelta :: a -> Position

instance PositionDelta Player where
  generateDelta player@(Player {..})  | newSpeed < playerDefaultSpeed = (0,0)
                                      | otherwise = newPos
    where 
      newPos    | newSpeed < playerMaxSpeed        = getNewPosition playerAngle newSpeed 
                | otherwise                     = getNewPosition playerAngle playerMaxSpeed 
      newSpeed     = accelerate playerMoveDuration playerSpeed playerAcceleration

instance PositionDelta Projectile where
  generateDelta projectile@(Projectile {..}) = getNewPosition projectileAngle projectileSpeed


--helper functions

--helper for rendering
renderCoordinate :: Angle -> Position -> (Scale, Scale) -> Position
renderCoordinate angle (x,y) (w,h) = (-1*(x2 + x),y2 + y)  
  where radians = (fromIntegral angle) * (1 * (pi/180))
        (x2,y2) = (((-1) * w * cos(radians)) - ((-1) * h * sin(radians)),((-1) * w * sin(radians)) + ((-1) * h * cos(radians)))


--helper for firing projectiles
adjustProjectile :: Projectile -> Angle -> Position-> Projectile
adjustProjectile projectile@(Projectile {..}) angle (x,y)  = projectile {
                                                            projectileAngle = ((-1)*angle),
                                                            projectilePosition = ((-1)*x,y)
                                                        }
--helper for Delta Position
getNewPosition :: Angle -> Speed -> Position
getNewPosition angle speed = (speed * sin(-1*(fromIntegral angle) * pi / (fromIntegral 180)), speed * cos(-1*(fromIntegral angle) * pi / (fromIntegral 180)))


accelerate :: Time -> Speed -> Acceleration -> Speed
accelerate duration speed acceleration | duration > 0 = speed + duration*acceleration
                                       | otherwise    = speed - (acceleration / 100)


-- Initial states for the world, projectiles and projectile                                                      
initialState :: World
initialState = World {
                state = Playing,
                score = 0,
                time = 0,
                screenSize = (400, 400),
                randomGen = mkStdGen 69,
                keys = empty,
                player = initialPlayer,
                projectiles = [],
                asteroids = []
                --bombs = []
            }

initialPlayer :: Player
initialPlayer = Player {
                  playerAngle = 0,
                  playerDeleted = False,
                  playerPosition = (0, 0),
                  playerSpeed = 5,
                  playerAcceleration = 10,
                  playerSprite = polygon [(6,0),(0,20),(-6,0)],
                  playerFireDelay = 0,
                  playerFireThreshold = 0.18,
                  playerDefaultSpeed = 5,
                  playerMaxSpeed     = 12,
                  playerScale        = 15,
                  playerMoveDuration = 0,
                  playerTurnRadius = 8
            }

initialProjectile :: Projectile
initialProjectile = Projectile {
                      projectileAngle = ((-1)*(playerAngle initialPlayer)),
                      projectileDeleted = False,
                      projectilePosition = (\(x,y) -> (-1*x,y)) (playerPosition initialPlayer),
                      projectileSpeed = 30,
                      projectileSprite = color red $ circleSolid 4,
                      projectileScale = 4
            }