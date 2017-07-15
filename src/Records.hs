{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Records where

import SDL.Vect (V2(..))
import Control.Lens.TH (makeFieldsNoPrefix)

data Trajectory = Trajectory
    { _t0 :: Double
    , _orig :: V2 Double
    , _period :: Double
    , _velocity :: V2 Double
    }

makeFieldsNoPrefix ''Trajectory

data CharacterType = Sapiens | Zombicus

data Character =
    Character { _characterId :: Int
              , _characterType :: CharacterType
              , _pos :: V2 Double
              , _velocity :: V2 Double
              }

makeFieldsNoPrefix ''Character
