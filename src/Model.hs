{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import Data.Set hiding (map, split, take, delete)


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


data State = Playing | Paused | GameOver
  deriving(Eq)


data World = World {
    state         :: State,
    score         :: Score,
    time          :: Time, 
    randomGen     :: StdGen,
    keys          :: Set Key,
    player        :: Player,
    projectiles   :: [Projectile],
    asteroids     :: [Asteroid],
    asteroidSpawn :: Time
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

instance Render Asteroid where
  render asteroid@(Asteroid {..}) = uncurry translate asteroidPosition $ color white $ circleSolid asteroidScale

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
          (w, h) = screenSize
          (xBound, yBound) = (((w / 2) + playerScale), ((h / 2) + playerScale))

instance Displace Projectile where
  displace projectile@(Projectile {..}) = projectile { projectilePosition = (x+dx, y+dy) }
    where 
      (x,y) = projectilePosition
      (dx, dy) = generateDelta projectile

instance Displace Asteroid where
  displace asteroid@(Asteroid {..})  = asteroid {asteroidPosition = newPosition }
    where 
          newPosition | x2 > xBound || x2 < (-1) * xBound = (-1*x2, y2)
                      | y2 > yBound || y2 < (-1) * yBound = (x2, -1*y2)
                      | otherwise                         = (x2, y2)
          (x,y) = asteroidPosition
          (dx, dy) = generateDelta asteroid
          (x2, y2) = (x+dx, y+dy)
          (w, h) = screenSize
          (xBound, yBound) = (((w / 2) + asteroidScale), ((h / 2) + asteroidScale))

--Class for handling position changes in regard to acceleration & speed changes
class PositionDelta a where
  generateDelta :: a -> Position

instance PositionDelta Player where
  generateDelta player@(Player {..})  | newSpeed < playerDefaultSpeed = (0,0)
                                      | otherwise = newPos
    where 
      newPos    | newSpeed < playerMaxSpeed        = getNewPosition playerAngle newSpeed 
                | otherwise                     = getNewPosition playerAngle playerMaxSpeed 
      newSpeed    = accelerate playerMoveDuration playerSpeed playerAcceleration

instance PositionDelta Projectile where
  generateDelta projectile@(Projectile {..}) = getNewPosition projectileAngle projectileSpeed

instance PositionDelta Asteroid where
  generateDelta asteroid@(Asteroid {..}) = getNewPosition asteroidAngle asteroidSpeed

class Collide a b where
  collide :: a -> b -> Maybe a

instance Collide Asteroid Projectile where
  collide asteroid@(Asteroid {..}) projectile@(Projectile {..})
      | (x2-x1)^2 + (y2-y1)^2 <= (asteroidScale+projectileScale)^2 = Just asteroid
      | otherwise = Nothing
    where
      (x1,y1) = asteroidPosition
      (x2,y2) = projectilePosition

instance Collide Asteroid Player where
  collide asteroid@(Asteroid {..}) player@(Player {..})
      | ((-1)*x4-x3)^2 + (y4-y3)^2 <= (asteroidScale+playerScale)^2 = Just asteroid
      | otherwise = Nothing
    where
      (x3,y3) = asteroidPosition
      (x4,y4) = playerPosition


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

-- constants                                     
screenSize :: Size                                     
screenSize = (400, 400)

-- Initial states for the world, projectiles and projectile                                                      
initialState :: StdGen -> World
initialState r = World {
                state = Playing,
                score = 0,
                time = 0,
                randomGen = r,
                keys = empty,
                player = initialPlayer,
                projectiles = [],
                asteroids = [],
                asteroidSpawn = 0
                
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
                      projectileSprite = color red $ circleSolid (projectileScale initialProjectile),
                      projectileScale = 4
            }


initialAsteroid :: Asteroid
initialAsteroid = Asteroid {
                    asteroidAngle = 0,
                    asteroidDeleted = False,
                    asteroidPosition = (0,0),
                    asteroidSpeed = 7,
                    asteroidSprite = color white $ circleSolid 8,
                    asteroidScale = 8
}

--generate random Asteroid 

generateAsteroid  :: StdGen -> Asteroid
generateAsteroid seed = Asteroid { 
                          asteroidAngle = newAngle,
                          asteroidDeleted = False,
                          asteroidPosition = newPosition,
                          asteroidSpeed = newSpeed,
                          asteroidSprite = color white $ circleSolid newScale,
                          asteroidScale = newScale*2.5
}
    where 
      (s1:s2:s3:_) = take 3 (unfoldr (Just . split) seed)
      newAngle  = randomInt 0 360 s2
      newPosition  = (fromIntegral $ randomInt (-200) 200 s3, fromIntegral $ randomInt (-200) 200 s3)
      newSpeed     = fromIntegral $ randomInt 5 15 s1
      newScale     = fromIntegral $ randomInt 4 9 s1
          
randomInt  :: Int -> Int -> StdGen -> Int
randomInt  start end seed = fst $ randomR (start, end) seed