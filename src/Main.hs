{-# LANGUAGE RankNTypes, OverloadedStrings, RecursiveDo #-}
import ReflexHost
import Records
import Rendering
import SimpleHomoSapiens
import HomoZombicus

import System.Random (StdGen, getStdGen, split)
import Control.Monad.Fix (MonadFix)
import Data.List (sortOn)
import Data.Traversable (for)
import Foreign.C.Types (CInt)

import Reflex

import SDL.Vect (V2(..), _y)
import qualified SDL

import Control.Lens ((^.))

windowSize :: V2 CInt
windowSize = V2 700 500

generators :: StdGen -> [StdGen]
generators g = let (g', g'') = split g in g' : generators g''

createCharacters :: (Reflex t, MonadHold t m, MonadFix m) =>
    StdGen -> Behavior t Double -> Event t () -> Behavior t [Character] ->
    m (Behavior t [Character])
createCharacters rnd time eTick scene = do
    let positions = V2 <$> [100, 200, 300, 400, 500, 600] <*> [150, 300, 450]
    characters <- for (zip3 positions [0..] $ generators rnd) $ \(cpos, cid, gen) ->
        if cid /= 3 && cid /= 6 && cid /= 7 then
            simpleHomoSapiens time eTick gen cid cpos
        else do
            (char, _eBite) <- homoZombicus time eTick cid cpos scene
            return char
    return $ sequenceA characters

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
