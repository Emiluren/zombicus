{-# LANGUAGE RankNTypes, DuplicateRecordFields, RecursiveDo #-}
import ReflexHost

import System.Random
import Control.Monad.Fix (MonadFix)

import Reflex

import SDL.Vect (V2(..), V4(..), (^*))
import SDL (($=), Point(P))
import qualified SDL
import qualified SDL.Image

import ReflexHost (SdlApp)

data RenderData = RenderData

data CharacterType = Sapiens | Zombicus

data Character =
    Character { characterId :: Int
              , characterType :: CharacterType
              , pos :: V2 Float
              , velocity :: V2 Float
              }

data Trajectory =
    Trajectory { rng :: StdGen
               , t0 :: Float
               , orig :: V2 Float
               , period :: Float
               , angle :: Float
               , velocity :: V2 Float
               }

positionAt :: Trajectory -> Float -> V2 Float
positionAt (Trajectory {t0 = startTime, orig = origPos, velocity = vel}) t =
    vel ^* (t - startTime) + origPos

simpleHomoSapiens :: (Reflex t, MonadHold t m, MonadFix m) => Int -> V2 Float -> Behavior t Float -> Event t () -> StdGen -> m (Behavior t Character)
simpleHomoSapiens self posInit time eTick rng = do
    let speed = 80
        rn1:rn2:_ = randoms rng :: [Float]
    return . pure $
        Character { characterId = self
                  , characterType = Sapiens
                  }

reflexGuest :: StdGen -> SdlApp RenderData
reflexGuest rnd e = do
    d <- foldDyn (:) [] e
    return $ pure RenderData

render :: RenderData -> IO ()
render _ = return ()

main :: IO ()
main = do
    rnd <- getStdGen
    reflexHost (reflexGuest rnd) render
