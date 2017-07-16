{-# LANGUAGE RankNTypes, RecursiveDo, OverloadedStrings, DuplicateRecordFields #-}
import ReflexHost
import Records

import System.Random (StdGen, getStdGen, split, randoms)
import Control.Monad.Fix (MonadFix)
import Data.List (sortOn)

import Reflex

import SDL.Vect (V2(..), V4(..), (^*), _y)
import SDL (($=), Point(P))
import qualified SDL
import qualified SDL.Image

import Control.Lens ((^.))

data RenderData = RenderData [Character]

speed :: Double
speed = 80

hitsObstacle :: V2 Double -> Bool
hitsObstacle (V2 x y) = x < 0 || x > 747 || y < 0 || y > 504

step :: Double
step = 0.02

positionAt :: Trajectory -> Double -> V2 Double
positionAt traj t =
    traj^.velocity ^* (t - traj^.t0) + traj^.orig

changeTrajectory :: (Reflex t) =>
    Behavior t Trajectory -> Behavior t Double -> () -> PushM t (Maybe ())
changeTrajectory traj time () = do
    currentTraj <- sample traj
    t <- sample time
    if t - currentTraj^.t0 >= currentTraj^.period ||
        hitsObstacle (positionAt currentTraj$ t + step)
    then return $ Just ()
    else return Nothing

-- Attempts to find a random trajectory 10 times that doesn't walk into an obstacle
makeTrajectory :: StdGen -> Double -> V2 Double -> Trajectory
makeTrajectory rng t0_ orig_ = findNew 10
    where
        findNew :: Int -> Trajectory
        findNew attempts =
            let rndPeriod:rndAngle:_ = randoms rng
                angle = rndAngle * pi * 2
                traj = Trajectory
                    { _t0 = t0_
                    , _orig = orig_
                    , _period = rndPeriod + 0.5
                    , _velocity = V2 (sin angle) (cos angle) ^* speed
                    }
            in if hitsObstacle (positionAt traj $ t0_ + step*2) && attempts > 1 then
                findNew (attempts - 1)
            else
                traj

newTrajectory :: (Reflex t) =>
    Behavior t Trajectory -> Behavior t Double -> StdGen ->
    () -> PushM t (StdGen, Trajectory)
newTrajectory traj time gen () = do
    let (g, g') = split gen
    t <- sample time
    currentTraj <- sample traj
    return (g', makeTrajectory g t $ positionAt currentTraj t)

generators :: StdGen -> [StdGen]
generators g = let (g', g'') = split g in g' : generators g''

simpleHomoSapiens :: (Reflex t, MonadHold t m, MonadFix m) =>
    Behavior t Double -> Event t () -> StdGen -> Int -> V2 Double -> m (Behavior t Character)
simpleHomoSapiens time eTick rng self posInit= do
    let (rng1, rng2) = split rng
    currentTime <- sample time

    rec
        -- Direction should be changed when traj.period has passed
        let eChange = push (changeTrajectory traj time) eTick

        -- Pick new direction at random
        eTraj <- mapAccumM_ (newTrajectory traj time) rng2 eChange

        -- Start with a random trajectory and pick a new one every period seconds
        traj <- hold (makeTrajectory rng1 currentTime posInit) eTraj

    let position = positionAt <$> traj <*> time
        vel = (^.velocity) <$> traj
    return $ Character self Sapiens <$> position <*> vel

reflexGuest :: StdGen -> SdlApp RenderData
reflexGuest rnd _eSdlEvent eTick time = do
    -- Create a few characters spread out over the board
    let startPositions = zipWith V2 [100,200..] [150,300..]
    chars <- sequenceA $
        zipWith3 (simpleHomoSapiens time eTick)
             (generators rnd) [0..2] startPositions

    -- Make sure characters are depth sorted
    return $ RenderData . sortOn (^.pos._y) <$> sequenceA chars

loadTextures :: SDL.Renderer -> IO Textures
loadTextures renderer =
    Textures
    <$> load "images/homo-sapien-left.png"
    <*> load "images/homo-sapien-right.png"
    <*> load "images/homo-zombicus-left.png"
    <*> load "images/homo-zombicus-right.png"
    where
        load = SDL.Image.loadTexture renderer

renderCharacter :: SDL.Renderer -> Textures -> Character -> IO ()
renderCharacter renderer textures character = do
    let cpos = SDL.P $ floor <$> character^.pos
        size = V2 53 96
        destRect = SDL.Rectangle cpos size
        (V2 vx _) = character^.velocity
        sprite = case character^.characterType of
            Sapiens -> if vx < 0 then humanLeft else humanRight
            Zombicus -> if vx < 0 then zombieLeft else zombieRight
    SDL.copy renderer (textures^.sprite) Nothing (Just destRect)


render :: SDL.Renderer -> Textures -> RenderData -> IO ()
render renderer textures (RenderData chars) = do
    SDL.rendererDrawColor renderer $= V4 200 200 200 255
    SDL.clear renderer

    mapM_ (renderCharacter renderer textures) chars

    SDL.present renderer


main :: IO ()
main = do
    SDL.initializeAll
    window <- SDL.createWindow "Zombicus" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    textures <- loadTextures renderer

    rnd <- getStdGen
    reflexHost (reflexGuest rnd) (render renderer textures)
