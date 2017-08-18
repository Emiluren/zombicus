{-# LANGUAGE RankNTypes, OverloadedStrings, RecursiveDo, ScopedTypeVariables #-}
import qualified ReflexHost
import Records
import qualified Rendering
import qualified World
import SimpleHomoSapiens
import HomoZombicus

import System.Random (StdGen, getStdGen, split)
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Data.List (sortOn)
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map

import Reflex

import SDL.Vect (V2(..), _y, (^/))
import qualified SDL

import Control.Lens ((^.), (%~), (&))

windowSize :: Num a => V2 a
windowSize = V2 700 500

bitableHomoSapiens :: forall t m. (Reflex t, MonadHold t m, MonadFix m) =>
    Behavior t Double -> Event t () -> Event t [Int] -> StdGen -> Int -> V2 Double -> Behavior t [Character] ->
    m (Behavior t Character, Event t Int)
bitableHomoSapiens time eTick eAllBites rng self posInit scene = do
    h <- simpleHomoSapiens time eTick rng self posInit
    eBiteMe <- headE $ ffilter (self `elem`) eAllBites

    let becomeZombie :: a -> PushM t (Behavior t Character, Event t Int)
        becomeZombie _id = do
            ch <- sample h
            homoZombicus time eTick self (ch^.pos) scene

        eBecome = pushAlways becomeZombie eBiteMe

    character <- hold h (fst <$> eBecome)
    eBite <- hold never (snd <$> eBecome)
    return (join character, switch eBite)

periodicTimer :: (Reflex t, MonadHold t m, MonadFix m) =>
    Behavior t Double -> Event t () -> Double -> m (Event t ())
periodicTimer time eTick timerPeriod = do
    timerT0 <- (+ timerPeriod) <$> sample time
    rec
        let eAlarm = push (\() -> do
                alarm <- sample tAlarm
                t <- sample time
                if t >= alarm then
                    return $ Just $ t + timerPeriod
                else
                    return Nothing) eTick
        tAlarm <- hold timerT0 eAlarm
    return $ () <$ eAlarm

newGameState :: GameState t
newGameState = GameState
    { _chars = Map.empty
    , _eCharacterBites = Map.empty
    , _eDestroys = Map.empty
    }

addCharacter ::
    Int -> GameState t -> Behavior t Character -> Event t Int -> Event t Int -> GameState t
addCharacter nextID state chr eBite eDestroy =
    state & chars %~ (Map.insert nextID chr)
          & eCharacterBites %~ (Map.insert nextID eBite)
          & eDestroys %~ (Map.insert nextID eDestroy)

removeCharacter :: Int -> GameState t -> GameState t
removeCharacter cid state =
    state & chars %~ (Map.delete cid)
          & eCharacterBites %~ (Map.delete cid)
          & eDestroys %~ (Map.delete cid)

removeCharacters :: [Int] -> GameState t -> GameState t
removeCharacters ids st = foldr removeCharacter st ids

fallDownHole :: Reflex t => Int -> Event t () -> Behavior t Character -> Event t Int
fallDownHole self eTick character =
    push (\() -> do
        c <- sample character
        if World.hitsHole (c^.pos) then
            return $ Just self
        else
            return Nothing) eTick

spawnNewHuman :: forall t. (Reflex t) =>
    Behavior t Double -> Event t () -> Behavior t [Character] -> Event t [Int] ->
    (StdGen, Int) -> () -> PushM t ((StdGen, Int), GameState t -> GameState t)
spawnNewHuman time eTick scene eAllBites (gen, cid) () = do
    let (g, g') = split gen
        center = windowSize ^/ 2

    (hcharacter, hbite) <-
        bitableHomoSapiens time eTick eAllBites g cid center scene

    let stateUpdate st =
            addCharacter cid st hcharacter hbite $ fallDownHole cid eTick hcharacter

    return ((g', cid+1), stateUpdate)

createCharacters :: forall t m. (Reflex t, MonadHold t m, MonadFix m) =>
    StdGen -> Behavior t Double -> Event t () ->
    m (Behavior t [Character])
createCharacters rnd time eTick = do
    let firstID = 0
    eSpawnTick <- periodicTimer time eTick 6

    rec
        -- Start with a single zombie
        (zCharacter, zBite) <- homoZombicus time eTick firstID (V2 36 322) scene
        let initState = addCharacter firstID newGameState zCharacter zBite $
                fallDownHole firstID eTick zCharacter

        -- Create events that adds new humans on a fixed interval and one that removes
        -- those that fall down holes
        eAdd <- mapAccumM_ (spawnNewHuman time eTick scene eAllBites) (rnd, 1) eSpawnTick
        let eRemove = removeCharacters <$> eDestroy

            -- Merge add and remove events to one stream
            eChange :: Event t (GameState t -> GameState t)
            eChange = mergeWith (\f1 f2 a -> f1 $ f2 a) [eAdd, eRemove]

        -- Apply add and remove events to the initial state
        (state :: Behavior t (GameState t))
            <- hold initState $ (\st f -> f st) <$> state <@> eChange

        let scene :: Behavior t [Character]
            scene = join $ (\st -> sequenceA $ Map.elems $ st^.chars) <$> state

            eAllBites :: Event t [Int]
            eAllBites = switch $
                (\st -> toList <$> (mergeList $ Map.elems $ st^.eCharacterBites)) <$> state

            eDestroy :: Event t [Int]
            eDestroy = switch $
                (\st -> toList <$> (mergeList $ Map.elems $ st^.eDestroys)) <$> state

    return scene

reflexGuest :: StdGen -> ReflexHost.SdlApp Rendering.RenderData
reflexGuest rnd _eSdlEvent eTick time = do
    -- Create a few characters spread out over the board
    scene <- createCharacters rnd time eTick

    -- Make sure characters are depth sorted
    return $ Rendering.RenderData . sortOn (^.pos._y) <$> scene

main :: IO ()
main = do
    SDL.initializeAll
    window <- SDL.createWindow "Zombicus" SDL.defaultWindow
        { SDL.windowInitialSize = windowSize
        }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    textures <- Rendering.loadTextures renderer

    rnd <- getStdGen
    ReflexHost.reflexHost (reflexGuest rnd) (Rendering.render renderer textures)
