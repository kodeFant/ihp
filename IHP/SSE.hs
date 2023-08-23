{-|
Module: IHP.SSE
Description: Building blocks for SSE applications
Copyright: (c) digitally induced GmbH, 2020
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module IHP.SSE
( SSEApp (..)
, startSSEApp
, setState
, getState
)
where

import IHP.Prelude
import IHP.ApplicationContext
import IHP.Controller.RequestContext
import qualified Data.UUID as UUID
import qualified Data.Maybe as Maybe
import qualified Control.Exception as Exception
import IHP.Controller.Context
import qualified Data.Aeson as Aeson
import qualified IHP.Log as Log
import qualified Control.Concurrent.Async as Async
import Network.Wai.EventSource

class SSEApp state where
    initialState :: state

    run :: (?state :: IORef state, ?context :: ControllerContext, ?applicationContext :: ApplicationContext, ?modelContext :: ModelContext) => IO ()
    run = pure ()

    onHeartbeat :: (?state :: IORef state, ?context :: ControllerContext, ?applicationContext :: ApplicationContext, ?modelContext :: ModelContext) => IO ()
    onHeartbeat = pure ()

    onClose :: (?state :: IORef state, ?context :: ControllerContext, ?applicationContext :: ApplicationContext, ?modelContext :: ModelContext) => IO ()
    onClose = pure ()

startSSEApp :: forall state. (SSEApp state, ?applicationContext :: ApplicationContext, ?requestContext :: RequestContext, ?context :: ControllerContext, ?modelContext :: ModelContext) => IO ()
startSSEApp  = do
    state <- newIORef (initialState @state)
    lastHeartbeatAt <- getCurrentTime >>= newIORef

    let ?state = state

    let heartBeathandler = do
            seconds <- secondsSinceLastHeartbeat lastHeartbeatAt
            when (seconds > pingWaitTime * 2) (throwIO HearbeatTimeout)
            onHeartbeat @state
    pure ()

    -- result <- Exception.try ((withHeartbeatThread pingWaitTime heartBeathandler (run @state)) `Exception.finally` onClose @state)
    -- case result of
    --     Left (e@Exception.SomeException{}) ->
    --         case Exception.fromException e of
    --             (Just Websocket.ConnectionClosed) -> pure ()
    --             (Just (Websocket.CloseRequest {})) -> pure ()
    --             (Just other) -> error ("Unhandled Websocket exception: " <> show other)
    --             Nothing -> Log.error (tshow e)
    --     Right _ -> pure ()


setState :: (?state :: IORef state) => state -> IO ()
setState newState = writeIORef ?state newState

getState :: (?state :: IORef state) => IO state
getState = readIORef ?state

-- | Json encode a payload and send it over the websocket wire
--
-- __Example:__
--
-- > message <- Aeson.decode <$> receiveData @LByteString
-- >
-- > case message of
-- >     Just decodedMessage -> handleMessage decodedMessage
-- >     Nothing -> sendJSON FailedToDecodeMessageError
--

data HearbeatTimeout
    = HearbeatTimeout
    deriving (Show)

instance Exception HearbeatTimeout

pingWaitTime :: Int
pingWaitTime = 30


connectionOnPong :: IORef UTCTime -> IO ()
connectionOnPong lastHeartbeatAt = do
    now <- getCurrentTime
    writeIORef lastHeartbeatAt now

secondsSinceLastHeartbeat :: IORef UTCTime -> IO Int
secondsSinceLastHeartbeat lastHeartbeatAt = do
    now <- getCurrentTime
    last <- readIORef lastHeartbeatAt
    pure $ ceiling $ nominalDiffTimeToSeconds $ diffUTCTime now last
