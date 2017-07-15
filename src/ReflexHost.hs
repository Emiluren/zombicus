{-# LANGUAGE RankNTypes #-}
module ReflexHost (SdlApp, reflexHost) where
-- Based on https://github.com/reflex-frp/reflex-platform/blob/develop/examples/host.hs

import Reflex
import Reflex.Host.Class
    ( newEventWithTriggerRef
    , runHostFrame
    , fireEvents
    , MonadReflexHost
    , EventTrigger
    )

import Control.Monad (unless)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (readIORef, IORef)
import Data.Dependent.Sum (DSum ((:=>)))

import qualified SDL
import qualified SDL.Time

type SdlApp renderData =
    forall t m. (Reflex t, MonadHold t m, MonadFix m) =>
    Event t SDL.Event -> Event t () -> Behavior t Double ->
    m (Behavior t renderData)

handleTrigger :: forall m t a.
    (MonadIO m, MonadReflexHost t m) =>
    IORef (Maybe (EventTrigger t a))
    -> a -> m ()
handleTrigger triggerRef e = do
    mETrigger <- liftIO $ readIORef triggerRef
    case mETrigger of
        -- No one is listening, ignore
        Nothing -> return ()
        -- Fire event for listeners
        Just eTrigger -> fireEvents [eTrigger :=> Identity e]

shouldQuit :: [SDL.Event] -> Bool
shouldQuit = elem SDL.QuitEvent . map SDL.eventPayload

reflexHost :: SdlApp renderData -> (renderData -> IO ()) -> IO ()
reflexHost myGuest renderFunction =
    runSpiderHost $ do
        -- Set up event callbacks
        (eSdlEvent, eSdlEventTriggerRef) <- newEventWithTriggerRef
        (eTick, eTickTimeTriggerRef) <- newEventWithTriggerRef

        -- Create a behavior with the current time
        time <- runHostFrame $ do
            t0 <- SDL.Time.time
            hold t0 $ pushAlways (const SDL.Time.time) eTick

        -- Set up FRP network
        bRenderData <- runHostFrame $ myGuest eSdlEvent eTick time

        let
            mainLoop = do
                sdlEvents <- SDL.pollEvents

                unless (shouldQuit sdlEvents) $ do
                    -- Trigger Reflex event for every SDL event
                    mapM_ (handleTrigger eSdlEventTriggerRef) sdlEvents

                    -- Tick the guest application with the current time
                    handleTrigger eTickTimeTriggerRef ()

                    -- Get current state and render
                    renderData <- runHostFrame $ sample bRenderData
                    liftIO (renderFunction renderData) >> mainLoop
        mainLoop
