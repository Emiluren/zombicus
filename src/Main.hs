{-# LANGUAGE RankNTypes, DuplicateRecordFields, RecursiveDo, OverloadedStrings #-}
import ReflexHost

import System.Random
import Control.Monad (forM_)
import Control.Monad.Fix (MonadFix)

import Reflex

import SDL.Vect (V2(..), V4(..), (^*))
import SDL (($=), Point(P))
import qualified SDL
import qualified SDL.Image

data RenderData = RenderData [Character]

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
positionAt Trajectory {t0 = startTime, orig = origPos, velocity = vel} t =
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
    Behavior t Double -> Event t () -> StdGen -> Int -> V2 Double -> m (Behavior t Character)
simpleHomoSapiens time eTick rng self posInit= do
    let gt':gt:_ = generators rng
    t <- sample time

    rec
        let eChange = push (changeTrajectory traj time) eTick
        eTraj <- mapAccumM_ (newTrajectory traj time) gt eChange
        traj <- hold (makeTrajectory gt' t posInit) eTraj

    let p = positionAt <$> traj <*> time
        v = (velocity :: Trajectory -> V2 Double) <$> traj
    return $ Character self Sapiens <$> p <*> v

reflexGuest :: StdGen -> SdlApp RenderData
reflexGuest rnd _eSdlEvent eTick time = do
    let startPositions = zipWith V2 [100,200..] [150,300..]
    chars <- sequenceA $
        zipWith3 (simpleHomoSapiens time eTick)
             (generators rnd) [0..3] startPositions
    return $ RenderData <$> sequenceA chars

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
                let cpos = SDL.P $ floor <$> pos c
                    size = V2 53 96
                    destRect = SDL.Rectangle cpos size
                    (V2 vx _) = velocity (c :: Character)
                    sprite = case characterType c of
                        Sapiens -> if vx < 0 then humanLeft else humanRight
                        Zombicus -> if vx < 0 then zombieLeft else zombieRight
                SDL.copy renderer sprite Nothing (Just destRect)
            SDL.present renderer

    rnd <- getStdGen
    reflexHost (reflexGuest rnd) render
