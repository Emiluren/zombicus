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

import Control.Monad (forever)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (readIORef, IORef)
import Data.Dependent.Sum (DSum ((:=>)))

import qualified SDL

type SdlApp renderData =
    forall t m. (Reflex t, MonadHold t m, MonadFix m) => Event t SDL.Event -> m (Behavior t renderData)

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

reflexHost :: SdlApp renderData -> (renderData -> IO ()) -> IO ()
reflexHost myGuest renderFunction =
    runSpiderHost $ do
        -- Set up event callbacks
        (e, eTriggerRef) <- newEventWithTriggerRef
        b <- runHostFrame $ myGuest e

        -- Main loop
        forever $ do
            sdlEvents <- SDL.pollEvents

            -- Trigger Reflex event for every SDL event
            mapM_ (handleTrigger eTriggerRef) sdlEvents

            -- Get current state and render
            output <- runHostFrame $ sample b
            liftIO $ renderFunction output
