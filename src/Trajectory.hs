{-# LANGUAGE TemplateHaskell #-}
module Trajectory where

import SDL.Vect (V2(..))
import Control.Lens (makeLenses)

data Trajectory = Trajectory
    { _t0 :: Double
    , _orig :: V2 Double
    , _period :: Double
    , _velocity :: V2 Double
    }

makeLenses ''Trajectory
