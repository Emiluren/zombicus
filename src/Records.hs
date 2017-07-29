{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Records where

import qualified SDL
import SDL.Vect (V2(..))
import Control.Lens.TH (makeFieldsNoPrefix)

data Trajectory = Trajectory
    { _t0 :: Double
    , _orig :: V2 Double
    , _period :: Double
    , _velocity :: V2 Double
    }

makeFieldsNoPrefix ''Trajectory

data ZombieState = ZombieState
    { _t0 :: Double
    , _orig :: V2 Double
    , _velocity :: V2 Double
    }

makeFieldsNoPrefix ''ZombieState

data CharacterType = Sapiens | Zombicus deriving Eq

data Character = Character
    { _characterId :: Int
    , _characterType :: CharacterType
    , _pos :: V2 Double
    , _velocity :: V2 Double
    }

makeFieldsNoPrefix ''Character

data Textures = Textures
    { _humanLeft :: SDL.Texture
    , _humanRight :: SDL.Texture
    , _zombieLeft :: SDL.Texture
    , _zombieRight :: SDL.Texture
    }

makeFieldsNoPrefix ''Textures
