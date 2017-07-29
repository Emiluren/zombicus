{-# LANGUAGE DuplicateRecordFields #-}
module HomoZombicus (homoZombicus) where
import Records

import Control.Monad.Fix (MonadFix)
import Data.List (minimumBy)

import Reflex

import SDL.Vect (V2(..), (*^), qd, distance, signorm)

import Control.Lens ((^.))

nearest :: V2 Double -> Int -> [Character] -> Maybe Character
nearest _ _ [_] = Nothing
nearest pos_ self scene =
    let
        distToOther :: Character -> Double
        distToOther c = qd pos_ $ c^.pos

        compareDistance :: Character -> Character -> Ordering
        compareDistance c1 c2 = compare (distToOther c1) (distToOther c2)

        notSelf :: Character -> Bool
        notSelf c = c^.characterId /= self

        closest :: Character
        closest = minimumBy compareDistance $ filter notSelf scene
    in
        if closest^.characterType == Zombicus && distance (closest^.pos) pos_ > 60 then
            Nothing
        else
            Just closest

zombieSpeed :: Character -> Double
zombieSpeed char =
    let
        baseSpeed = 20.0
    in
        if char^.characterType == Sapiens then
            baseSpeed
        else
            -baseSpeed

makeZombieState :: Double -> V2 Double -> Int -> [Character] -> ZombieState
makeZombieState t0_ orig_ self scene =
    let
        vel =
            case nearest orig_ self scene of
                Just other ->
                    zombieSpeed other *^ signorm (other^.pos - orig_)
                Nothing ->
                    V2 0 0
    in
        ZombieState { _t0 = t0_
                    , _velocity = vel
                    , _orig = orig_
                    }

homoZombicus :: (Reflex t, MonadHold t m, MonadFix m) =>
    Behavior t Double -> Event t () -> Int -> V2 Double -> Behavior t [Character] ->
    m (Behavior t Character, Event t Int)
homoZombicus time eTick self posInit scene =
    undefined
