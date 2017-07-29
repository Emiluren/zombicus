{-# LANGUAGE RankNTypes, OverloadedStrings #-}
import ReflexHost
import Records
import Rendering
import SimpleHomoSapiens

import System.Random (StdGen, getStdGen, split)
import Data.List (sortOn)

import SDL.Vect (V2(..), _y)
import qualified SDL

import Control.Lens ((^.))

generators :: StdGen -> [StdGen]
generators g = let (g', g'') = split g in g' : generators g''

reflexGuest :: StdGen -> SdlApp RenderData
reflexGuest rnd _eSdlEvent eTick time = do
    -- Create a few characters spread out over the board
    let startPositions = zipWith V2 [100,200..] [150,300..]
    chars <- sequenceA $
        zipWith3 (simpleHomoSapiens time eTick)
             (generators rnd) [0..2] startPositions

    -- Make sure characters are depth sorted
    return $ RenderData . sortOn (^.pos._y) <$> sequenceA chars

main :: IO ()
main = do
    SDL.initializeAll
    window <- SDL.createWindow "Zombicus" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    textures <- loadTextures renderer

    rnd <- getStdGen
    reflexHost (reflexGuest rnd) (render renderer textures)
