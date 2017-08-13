{-# LANGUAGE RankNTypes, OverloadedStrings, RecursiveDo, ScopedTypeVariables #-}
import ReflexHost
import Records
import Rendering
import SimpleHomoSapiens
import HomoZombicus

import System.Random (StdGen, getStdGen, split)
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Data.List (sortOn)
import Data.List.NonEmpty (toList)
import Data.Traversable (for)
import Foreign.C.Types (CInt)

import Reflex

import SDL.Vect (V2(..), _y)
import qualified SDL

import Control.Lens ((^.))

windowSize :: V2 CInt
windowSize = V2 700 500

bitableHomoSapiens :: forall t m. (Reflex t, MonadHold t m, MonadFix m) =>
    Behavior t Double -> Event t () -> Event t [Int] -> StdGen -> Int -> V2 Double -> Behavior t [Character] ->
    m (Behavior t Character, Event t Int)
bitableHomoSapiens time eTick eAllBites rng self posInit scene = do
    h <- simpleHomoSapiens time eTick rng self posInit

    let eBiteMe :: Event t [Int]
        eBiteMe = ffilter (self `elem`) eAllBites

        becomeZombie :: a -> PushM t (Behavior t Character, Event t Int)
        becomeZombie _id = do
            ch <- sample h
            homoZombicus time eTick self (ch^.pos) scene

        eBecome = pushAlways becomeZombie eBiteMe

    character <- hold h (fst <$> eBecome)
    eBite <- hold never (snd <$> eBecome)
    return (join character, switch eBite)

generators :: StdGen -> [StdGen]
generators g = let (g', g'') = split g in g' : generators g''

createCharacters :: (Reflex t, MonadHold t m, MonadFix m) =>
    StdGen -> Behavior t Double -> Event t () -> Behavior t [Character] ->
    m (Behavior t [Character])
createCharacters rnd time eTick scene = do
    let positions = V2 <$> [100, 200, 300, 400, 500, 600] <*> [150, 300, 450]

    rec
        (characterBites :: [(Behavior t Character, Event t Int)])
            <- for (zip3 positions [0..] $ generators rnd) $ \(cpos, cid, gen) ->
                if cid /= 3 && cid /= 6 && cid /= 7 then do
                    bitableHomoSapiens time eTick eAllBites gen cid cpos scene
                else do
                    homoZombicus time eTick cid cpos scene

        let (characters, bites) = unzip characterBites
            eAllBites = toList <$> mergeList bites
    return (sequenceA characters)

reflexGuest :: StdGen -> SdlApp RenderData
reflexGuest rnd _eSdlEvent eTick time = do
    -- Create a few characters spread out over the board
    rec scene <- createCharacters rnd time eTick scene

    -- Make sure characters are depth sorted
    return $ RenderData . sortOn (^.pos._y) <$> scene

main :: IO ()
main = do
    SDL.initializeAll
    window <- SDL.createWindow "Zombicus" SDL.defaultWindow
        { SDL.windowInitialSize = windowSize
        }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    textures <- loadTextures renderer

    rnd <- getStdGen
    reflexHost (reflexGuest rnd) (render renderer textures)
