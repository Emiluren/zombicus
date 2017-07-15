{-# LANGUAGE RankNTypes, RecursiveDo, OverloadedStrings, DuplicateRecordFields #-}
import ReflexHost
import Records

import System.Random
import Control.Monad (forM_)
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

makeTrajectory :: StdGen -> Double -> V2 Double -> Trajectory
makeTrajectory rng t0_ orig_ =
    let rndPeriod:rndAngle:_ = randoms rng :: [Double]
        angle = rndAngle * pi * 2
    in Trajectory { _t0 = t0_
                  , _orig = orig_
                  , _period = rndPeriod + 0.5
                  , _velocity = V2 (sin angle) (cos angle) ^* speed
                  }

positionAt :: Trajectory -> Double -> V2 Double
positionAt traj t =
    traj^.velocity ^* (t - traj^.t0) + traj^.orig

changeTrajectory :: (Reflex t) =>
    Behavior t Trajectory -> Behavior t Double -> () -> PushM t (Maybe ())
changeTrajectory traj time () = do
    traj_ <- sample traj
    t <- sample time
    if t - traj_^.t0 >= traj_^.period
        then return $ Just ()
        else return Nothing

newTrajectory :: (Reflex t) =>
    Behavior t Trajectory -> Behavior t Double -> StdGen ->
    () -> PushM t (StdGen, Trajectory)
newTrajectory traj time gen () = do
    let (g, g') = split gen
    t <- sample time
    traj_ <- sample traj
    return (g', makeTrajectory g t $ positionAt traj_ t)

generators :: StdGen -> [StdGen]
generators g = let (g', g'') = split g in g' : generators g''

simpleHomoSapiens :: (Reflex t, MonadHold t m, MonadFix m) =>
    Behavior t Double -> Event t () -> StdGen -> Int -> V2 Double -> m (Behavior t Character)
simpleHomoSapiens time eTick rng self posInit= do
    let (rng1, rng2) = split rng
    t <- sample time

    rec
        let eChange = push (changeTrajectory traj time) eTick
        eTraj <- mapAccumM_ (newTrajectory traj time) rng2 eChange
        traj <- hold (makeTrajectory rng1 t posInit) eTraj

    let p = positionAt <$> traj <*> time
        v = (^.velocity) <$> traj
    return $ Character self Sapiens <$> p <*> v

reflexGuest :: StdGen -> SdlApp RenderData
reflexGuest rnd _eSdlEvent eTick time = do
    let startPositions = zipWith V2 [100,200..] [150,300..]
    chars <- sequenceA $
        zipWith3 (simpleHomoSapiens time eTick)
             (generators rnd) [0..3] startPositions
    return $ RenderData . sortOn (^.pos._y) <$> sequenceA chars

main :: IO ()
main = do
    SDL.initializeAll
    window <- SDL.createWindow "Zombicus" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    humanLeft <- SDL.Image.loadTexture renderer "images/homo-sapien-left.png"
    humanRight <- SDL.Image.loadTexture renderer "images/homo-sapien-right.png"
    zombieLeft <- SDL.Image.loadTexture renderer "images/homo-zombicus-left.png"
    zombieRight <- SDL.Image.loadTexture renderer "images/homo-zombicus-right.png"

    let render :: RenderData -> IO ()
        render (RenderData chars) = do
            SDL.rendererDrawColor renderer $= V4 200 200 200 255
            SDL.clear renderer
            forM_ chars $ \c -> do
                let cpos = SDL.P $ floor <$> c^.pos
                    size = V2 53 96
                    destRect = SDL.Rectangle cpos size
                    (V2 vx _) = c^.velocity
                    sprite = case c^.characterType of
                        Sapiens -> if vx < 0 then humanLeft else humanRight
                        Zombicus -> if vx < 0 then zombieLeft else zombieRight
                SDL.copy renderer sprite Nothing (Just destRect)
            SDL.present renderer

    rnd <- getStdGen
    reflexHost (reflexGuest rnd) render
