module Rendering where

import Records
import qualified World

import qualified SDL
import SDL (($=))
import qualified SDL.Image
import SDL.Vect (V2(..), V4(..))

import Control.Lens ((^.))

data RenderData = RenderData [Character]

loadTextures :: SDL.Renderer -> IO Textures
loadTextures renderer =
    Textures
    <$> load "images/homo-sapien-left.png"
    <*> load "images/homo-sapien-right.png"
    <*> load "images/homo-zombicus-left.png"
    <*> load "images/homo-zombicus-right.png"
    where
        load = SDL.Image.loadTexture renderer

charWidth :: Num a => a
charWidth = 53

charHeight :: Num a => a
charHeight = 96

characterOffset :: V2 Double
characterOffset = V2 (charWidth / 2) (charHeight - 10)

renderCharacter :: SDL.Renderer -> Textures -> Character -> IO ()
renderCharacter renderer textures character = do
    let size = V2 charWidth charHeight
        pos' = (character^.pos) - characterOffset
        cpos = SDL.P $ floor <$> pos'
        destRect = SDL.Rectangle cpos size
        (V2 vx _) = character^.velocity
        sprite = case character^.characterType of
            Sapiens -> if vx < 0 then humanLeft else humanRight
            Zombicus -> if vx < 0 then zombieLeft else zombieRight
    SDL.copy renderer (textures^.sprite) Nothing (Just destRect)


render :: SDL.Renderer -> Textures -> RenderData -> IO ()
render renderer textures (RenderData characters) = do
    SDL.rendererDrawColor renderer $= V4 240 240 255 255
    SDL.clear renderer

    World.renderHoles renderer $ V4 200 200 240 255
    mapM_ (renderCharacter renderer textures) characters

    SDL.present renderer
