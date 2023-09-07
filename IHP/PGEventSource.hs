-- | The IHP.PGEventSource module is responsible for dispatching Server-sent Events (SSE) from PostgreSQL notification triggers.
module IHP.PGEventSource (streamPgEvent, initPgEventSource) where

import IHP.Prelude
import IHP.ApplicationContext (ApplicationContext(pgListener))
import IHP.Controller.Context (fromContext, putContext)
import IHP.ControllerSupport
import IHP.ModelSupport (withTableReadTracker, withRowLevelSecurityDisabled, sqlExec, trackTableRead)
import qualified IHP.Log as Log
import qualified IHP.PGListener as PGListener
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVar, writeTVar, modifyTVar', readTVarIO)
import qualified Control.Exception as Exception
import qualified Data.ByteString.Builder as B
import Data.String.Interpolate.IsString (i)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Database.PostgreSQL.Simple.Types as PG
import Database.PostgreSQL.Simple.Notification (notificationPid, Notification)
import qualified Network.Wai as Wai
import Network.HTTP.Types (status200, hConnection)
import Network.HTTP.Types.Header (HeaderName, hContentType, hCacheControl)
import qualified Data.Set as Set
import Data.String.Interpolate.Util (unindent)


-- | Initialize database events functionality. This makes the PGListener 
-- from the `ApplicationContext` available in the `ControllerContext`.
initPgEventSource :: (?context :: ControllerContext, ?applicationContext :: ApplicationContext) => IO ()
initPgEventSource = do
    putContext ?applicationContext.pgListener


-- | Stream database change events to clients as Server-sent Events (SSE).
-- This function dispatches events to the client (most commonly the web browser) when the PGListener subscription triggers a notification.
streamPgEvent :: (?modelContext :: ModelContext, ?context :: ControllerContext, ?touchedTables::IORef (Set ByteString)) => ByteString -> IO ()
streamPgEvent eventName  = do
    touchedTables <- Set.toList <$> readIORef ?touchedTables
    pgListener <- fromContext @PGListener.PGListener

    -- Keep track of whether the client is still connected
    isActive <- newTVarIO True
    -- Cleanup actions to be executed when the client disconnects
    cleanupActions <- newTVarIO [] :: IO (TVar [IO ()])

    let addCleanupAction action = atomically $ modifyTVar' cleanupActions (action:)

    let streamBody sendChunk flush = do
            -- For each touched table, create a trigger in the database and subscribe to notifications
            touchedTables
                |> mapM \table -> do
                    createTriggerForTable table

                    let notificationCallback = handleNotificationTrigger sendChunk flush eventName table
                    subscription <- PGListener.subscribe (channelName table) notificationCallback pgListener

                    -- Add a cleanup action to unsubscribe from the channel when the client disconnects
                    addCleanupAction $ PGListener.unsubscribe subscription pgListener

            -- Send a heartbeat to the client every 30 seconds to keep the connection alive
            sendHeartbeats sendChunk flush isActive
                `Exception.finally` runCleanupActions cleanupActions

    -- Send the stream to the client
    respondEventSource streamBody


-- | Required headers for SSE responses.
sseHeaders :: [(HeaderName, ByteString)]
sseHeaders =
    [ (hCacheControl, "no-store")
    , (hConnection, "keep-alive")
    , (hContentType, "text/event-stream")
    ]

-- | Responds with a streaming body as an SSE to the client.
-- This function takes a 'Wai.StreamingBody' (essentially a stream of data chunks)
-- and sends it to the client with the appropriate headers
respondEventSource :: (?context::ControllerContext) => Wai.StreamingBody -> IO ()
respondEventSource streamBody = respondAndExit $ Wai.responseStream status200 sseHeaders streamBody


-- | Send periodic heartbeats to the client to keep the connection alive.
sendHeartbeats :: (?context :: ControllerContext) => (B.Builder -> IO a) -> IO () -> TVar Bool -> IO ()
sendHeartbeats sendChunk flush isActive = do
    active <- readTVarIO isActive
    when active $ do
        threadDelay (30 * 1000000)
        handleDisconnect isActive $ do
            sendChunk (B.stringUtf8 ": heartbeat\n\n") >> flush
        sendHeartbeats sendChunk flush isActive


-- Gracefully handle the client disconnect exception
handleDisconnect :: (?context :: ControllerContext) =>  TVar Bool -> IO () -> IO ()
handleDisconnect isActive action = action `Exception.catch` \e ->
    if isDisconnectException e
        then do
            Log.info ("PGEventSource disconnected gracefully" :: Text)
            atomically $ writeTVar isActive False
        else Log.error $ "PGEventSource Error: " ++ show (e :: Exception.SomeException)
    where
        isDisconnectException e = "Client closed connection prematurely" `isInfixOf` show (e :: Exception.SomeException)


-- | Executes all cleanup actions stored in the provided 'TVar'.
-- 
-- After executing the cleanup actions, the 'TVar' is emptied.
-- 
-- @param cleanupActions A 'TVar' containing a list of IO actions representing cleanup operations.
runCleanupActions :: (?context :: ControllerContext) => TVar [IO a] -> IO ()
runCleanupActions cleanupActions = do
    actions <- atomically $ do
        a <- readTVar cleanupActions
        writeTVar cleanupActions []
        return a
    forM_ actions id
    Log.debug ("PGEventSource cleanup actions executed" :: Text)


-- | Handle notifications triggered by table changes. Sends the notification data as an SSE.
handleNotificationTrigger :: (?context :: ControllerContext) => (B.Builder -> IO a) -> IO () -> ByteString -> ByteString -> Notification -> IO ()
handleNotificationTrigger sendChunk flush eventName table notification = do
    -- See https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#event_stream_format
    let eventPayload =  
            B.byteString $ cs $ unindent (
                [i|
                id: #{fromIntegral $ notificationPid notification}
                event: #{eventName}
                data: #{table} change event triggered
                |] <> "\n\n" -- Ending with at least two newlines is required to separate the event payload from the next one in the stream.
            ) 

    sendChunk eventPayload >> flush
        `Exception.catch` (\e -> Log.error $ "PGEventSource error: Error sending chunk: " ++ show (e :: Exception.SomeException))
    pure ()



-- | Creates a database trigger that notifies on table changes (insert, update, delete).
createTriggerForTable :: (?modelContext::ModelContext) => ByteString -> IO ()
createTriggerForTable table = do
    let createTriggerSql = notificationTrigger table
    withRowLevelSecurityDisabled do
        sqlExec createTriggerSql ()
        pure ()

-- | Generate the channel name for PostgreSQL notifications based on the table name.
channelName :: ByteString -> ByteString
channelName tableName = "pge_did_change_" <> tableName


-- | Constructs the SQL for creating triggers on table changes and sending notifications to the corresponding channel.
notificationTrigger :: ByteString -> PG.Query
notificationTrigger tableName = PG.Query [i|
        BEGIN;
            CREATE OR REPLACE FUNCTION #{functionName}() RETURNS TRIGGER AS $$
                BEGIN
                    PERFORM pg_notify('#{channelName tableName}', '');
                    RETURN new;
                END;
            $$ language plpgsql;
            DROP TRIGGER IF EXISTS #{insertTriggerName} ON #{tableName};
            CREATE TRIGGER #{insertTriggerName} AFTER INSERT ON "#{tableName}" FOR EACH STATEMENT EXECUTE PROCEDURE #{functionName}();

            DROP TRIGGER IF EXISTS #{updateTriggerName} ON #{tableName};
            CREATE TRIGGER #{updateTriggerName} AFTER UPDATE ON "#{tableName}" FOR EACH STATEMENT EXECUTE PROCEDURE #{functionName}();

            DROP TRIGGER IF EXISTS #{deleteTriggerName} ON #{tableName};
            CREATE TRIGGER #{deleteTriggerName} AFTER DELETE ON "#{tableName}" FOR EACH STATEMENT EXECUTE PROCEDURE #{functionName}();
        
        COMMIT;
    |]
    where
        functionName = "pge_notify_did_change_" <> tableName
        insertTriggerName = "pge_did_insert_" <> tableName
        updateTriggerName = "pge_did_update_" <> tableName
        deleteTriggerName = "pge_did_delete_" <> tableName