{-# LANGUAGE TemplateHaskell #-}
module Character where

import SDL.Vect (V2(..))
import Control.Lens (makeLenses)

data CharacterType = Sapiens | Zombicus

data Character =
    Character { _characterId :: Int
              , _characterType :: CharacterType
              , _pos :: V2 Double
              , _velocity :: V2 Double
              }

makeLenses ''Character
