module Rendering where

import Records

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

renderCharacter :: SDL.Renderer -> Textures -> Character -> IO ()
renderCharacter renderer textures character = do
    let cpos = SDL.P $ floor <$> character^.pos
        size = V2 53 96
        destRect = SDL.Rectangle cpos size
        (V2 vx _) = character^.velocity
        sprite = case character^.characterType of
            Sapiens -> if vx < 0 then humanLeft else humanRight
            Zombicus -> if vx < 0 then zombieLeft else zombieRight
    SDL.copy renderer (textures^.sprite) Nothing (Just destRect)


render :: SDL.Renderer -> Textures -> RenderData -> IO ()
render renderer textures (RenderData chars) = do
    SDL.rendererDrawColor renderer $= V4 200 200 200 255
    SDL.clear renderer

    mapM_ (renderCharacter renderer textures) chars

    SDL.present renderer
