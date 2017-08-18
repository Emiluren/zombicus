{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Records where

import qualified SDL
import SDL.Vect (V2(..))
import Control.Lens.TH (makeFieldsNoPrefix)
import Data.Map.Strict (Map)
import Reflex (Behavior, Event)

data Trajectory = Trajectory
    { _t0 :: Double
    , _orig :: V2 Double
    , _period :: Double
    , _velocity :: V2 Double
    }

makeFieldsNoPrefix ''Trajectory

data CharacterType = Sapiens | Zombicus deriving Eq

data Character = Character
    { _characterId :: Int
    , _characterType :: CharacterType
    , _pos :: V2 Double
    , _velocity :: V2 Double
    }

makeFieldsNoPrefix ''Character

data ZombieState = ZombieState
    { _t0 :: Double
    , _orig :: V2 Double
    , _velocity :: V2 Double
    }

makeFieldsNoPrefix ''ZombieState

data GameState t = GameState
    { _chars :: Map Int (Behavior t Character)
    , _eCharacterBites :: Map Int (Event t Int)
    , _eDestroys :: Map Int (Event t Int)
    }

makeFieldsNoPrefix ''GameState

data Textures = Textures
    { _humanLeft :: SDL.Texture
    , _humanRight :: SDL.Texture
    , _zombieLeft :: SDL.Texture
    , _zombieRight :: SDL.Texture
    }

makeFieldsNoPrefix ''Textures
