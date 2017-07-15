{-# LANGUAGE RankNTypes, DuplicateRecordFields, RecursiveDo, OverloadedStrings #-}
import ReflexHost

import System.Random
import Control.Monad.Fix (MonadFix)

import Reflex

import SDL.Vect (V2(..), V4(..), (^*))
import SDL (($=), Point(P))
import qualified SDL
import qualified SDL.Image

data RenderData = RenderData Double

data CharacterType = Sapiens | Zombicus

data Character =
    Character { characterId :: Int
              , characterType :: CharacterType
              , pos :: V2 Double
              , velocity :: V2 Double
              }

data Trajectory =
    Trajectory { t0 :: Double
               , orig :: V2 Double
               , period :: Double
               , velocity :: V2 Double
               }

speed :: Double
speed = 80

makeTrajectory :: StdGen -> Double -> V2 Double -> Trajectory
makeTrajectory rng t0_ orig_ =
    let rndPeriod:rndAngle:_ = randoms rng :: [Double]
        angle = rndAngle * pi * 2
    in Trajectory { t0 = t0_
                  , orig = orig_
                  , period = rndPeriod + 0.5
                  , velocity = V2 (sin angle) (cos angle) ^* speed
                  }

positionAt :: Trajectory -> Double -> V2 Double
positionAt (Trajectory {t0 = startTime, orig = origPos, velocity = vel}) t =
    vel ^* (t - startTime) + origPos

changeTrajectory :: (Reflex t) =>
    Behavior t Trajectory -> Behavior t Double -> () -> PushM t (Maybe ())
changeTrajectory traj time () = do
    traj_ <- sample traj
    t <- sample time
    if t - t0 traj_ >= period traj_
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
    Int -> V2 Double -> Behavior t Double -> Event t () -> StdGen -> m (Behavior t Character)
simpleHomoSapiens self posInit time eTick rng = do
    let gt':gt:_ = generators rng
    rec
        let eChange = push (changeTrajectory traj time) eTick
        t <- sample time
        traj <- hold (makeTrajectory gt' t posInit) =<<
            mapAccumM_ (newTrajectory traj time) gt eChange

    traj_ <- sample traj
    return . pure $ Character { characterId = self
                              , characterType = Sapiens
                              , pos = positionAt traj_ t
                              , velocity = velocity (traj_ :: Trajectory)
                              }

reflexGuest :: StdGen -> SdlApp RenderData
reflexGuest rnd eSdlEvent eTick time = do
    d <- foldDyn (:) [] eSdlEvent
    return $ RenderData <$> time

render :: SDL.Renderer -> RenderData -> IO ()
render renderer (RenderData time) = do
    SDL.rendererDrawColor renderer $= V4 200 200 200 255
    SDL.clear renderer
    SDL.present renderer
    putStrLn $ "time = " ++ show time

main :: IO ()
main = do
    SDL.initializeAll
    window <- SDL.createWindow "Zombicus" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    rnd <- getStdGen
    reflexHost (reflexGuest rnd) (render renderer)
