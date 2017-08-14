{-# LANGUAGE DuplicateRecordFields, RecursiveDo #-}
module HomoZombicus (homoZombicus) where
import Records

import Control.Monad.Fix (MonadFix)
import Data.List (minimumBy)

import Reflex

import SDL.Vect (V2(..), (^*), (*^), qd, distance, signorm)

import Control.Lens ((^.))

nearest :: V2 Double -> Int -> [Character] -> Maybe Character
nearest pos_ self scene =
    let
        distToOther :: Character -> Double
        distToOther c = qd pos_ $ c^.pos

        compareDistance :: Character -> Character -> Ordering
        compareDistance c1 c2 = compare (distToOther c1) (distToOther c2)

        notSelf :: Character -> Bool
        notSelf c = c^.characterId /= self

        notFarAwayZombie :: Character -> Bool
        notFarAwayZombie c = c^.characterType /= Zombicus || distance (c^.pos) pos_ < 60

        possibleChoices :: [Character]
        possibleChoices = filter notSelf $ filter notFarAwayZombie scene
    in
        if null possibleChoices then
            Nothing
        else
            Just $ minimumBy compareDistance possibleChoices

nearestSapiens :: V2 Double -> Int -> [Character] -> Maybe Character
nearestSapiens pos_ self = nearest pos_ self . filter (\c -> c^.characterType == Sapiens)

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

positionAt :: ZombieState -> Double -> V2 Double
positionAt state t =
    state^.velocity ^* (t - state^.t0) + state^.orig

changeState :: (Reflex t) =>
    Int -> Behavior t Double -> Behavior t ZombieState -> Behavior t [Character] ->
    () -> PushM t (Maybe ZombieState)
changeState self time state scene () = do
    t <- sample time
    st <- sample state
    sc <- sample scene
    if t - st^.t0 >= 0.2 then
        return . Just $ makeZombieState t (positionAt st t) self sc
    else
        return Nothing

bite :: (Reflex t) =>
    Int -> Behavior t Double -> Behavior t ZombieState -> Behavior t [Character] ->
    () -> PushM t (Maybe Int)
bite self time state scene () = do
    t <- sample time
    st <- sample state
    sc <- sample scene
    let mVictim = nearestSapiens (st^.orig) self sc
        myPos = positionAt st t
    case mVictim of
        Just victim ->
            if distance (victim^.pos) myPos < 10 then
                return . Just $ victim^.characterId
            else
                return Nothing
        Nothing ->
            return Nothing

homoZombicus :: (Reflex t, MonadHold t m, MonadFix m) =>
    Behavior t Double -> Event t () -> Int -> V2 Double -> Behavior t [Character] ->
    m (Behavior t Character, Event t Int)
homoZombicus time eTick self posInit scene = do
    currentTime <- sample time

    rec
        let eChangeState = push (changeState self time state scene) eTick
        state <- hold (makeZombieState currentTime posInit self []) eChangeState

    let position = positionAt <$> state <*> time
        vel = (^.velocity) <$> state
        character = Character self Zombicus <$> position <*> vel
        eBite = push (bite self time state scene) eTick

    return (character, eBite)
