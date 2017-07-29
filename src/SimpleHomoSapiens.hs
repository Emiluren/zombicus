{-# LANGUAGE RecursiveDo, DuplicateRecordFields #-}
module SimpleHomoSapiens (simpleHomoSapiens) where
import Records

import System.Random (StdGen, split, randoms)
import Control.Monad.Fix (MonadFix)

import Reflex

import SDL.Vect (V2(..), (^*))

import Control.Lens ((^.))

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
