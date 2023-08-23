module IHP.LocalRefresh where

import IHP.Prelude
import IHP.LocalRefresh.Types
import IHP.ControllerSupport
import IHP.ApplicationContext
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import IHP.Controller.Session
import qualified Network.Wai.Internal as Wai
import qualified Data.Binary.Builder as ByteString
import qualified Data.Set as Set
import IHP.ModelSupport
import qualified Control.Exception as Exception
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import IHP.Controller.Context
import IHP.Controller.Response 
import qualified IHP.PGListener as PGListener
import qualified Database.PostgreSQL.Simple.Types as PG
import Data.String.Interpolate.IsString





newLocalRefreshServer :: PGListener.PGListener -> LocalRefreshServer
newLocalRefreshServer pgListener = LocalRefreshServer { subscriptions = [], sessions = [], subscribedTables = mempty, pgListener }

initLocalRefresh ::(?context :: ControllerContext, ?applicationContext :: ApplicationContext) => IO ()
initLocalRefresh = do
    putContext LocalRefreshDisabled
    putContext ?applicationContext.localRefreshServer

localRefresh :: (
    ?theAction :: action
    , Controller action
    , ?modelContext :: ModelContext
    , ?context :: ControllerContext
    ) => ((?modelContext :: ModelContext) => IO ()) -> IO ()
localRefresh runAction = do
    localRefreshState <- fromContext @LocalRefreshState
    localRefreshServer <- fromContext @(IORef LocalRefreshServer)

    case localRefreshState of
        LocalRefreshDisabled -> do
            availableSessions <- getAvailableSessions localRefreshServer

            id <- UUID.nextRandom

            -- We save the current state of the controller context here. This includes e.g. all current
            -- flash messages, the current user, ...
            --
            -- This frozen context is used as a "template" inside renderView to make a new controller context
            -- with the exact same content we had when rendering the initial page, whenever we do a server-side re-rendering
            frozenControllerContext <- freeze ?context

            let renderView = \requestContext -> do
                    controllerContext <- unfreeze frozenControllerContext
                    let ?context = controllerContext { requestContext }
                    action ?theAction

            putContext (LocalRefreshEnabled id)

            -- We save the allowed session ids to the session cookie to only grant a client access
            -- to sessions it initially opened itself
            --
            -- Otherwise you might try to guess session UUIDs to access other peoples local refresh sessions
            setSession "localRefreshSessions" (map UUID.toText (id:availableSessions) |> Text.intercalate "")

            withTableReadTracker do
                let handleResponse exception@(ResponseException response) = case response of
                        Wai.ResponseBuilder status headers builder -> do
                            tables <- readIORef ?touchedTables
                            lastHeartbeat <- getCurrentTime
                            lastResponse <- Exception.evaluate (ByteString.toLazyByteString builder)

                            event <- MVar.newEmptyMVar
                            let session = LocalRefreshSession { id,  event, tables, lastResponse, lastHeartbeat }
                            modifyIORef' (localRefreshServer) (\s -> (s { sessions = session:s.sessions } :: LocalRefreshServer) )
                            async (gcSessions localRefreshServer)

                            registerNotificationTrigger ?touchedTables localRefreshServer

                            throw exception
                        _   -> error "Unimplemented WAI response type."

                runAction `Exception.catch` handleResponse
        LocalRefreshEnabled {} -> do
            -- When this function calls the 'action ?theAction' in the other case
            -- we will evaluate this branch
            runAction


getAvailableSessions :: (?context :: ControllerContext) => IORef LocalRefreshServer -> IO [UUID]
getAvailableSessions localRefreshServer = do
    allSessions <- (.sessions) <$> readIORef localRefreshServer
    text <- fromMaybe "" <$> getSession "localRefreshSessions"
    let uuidCharCount = Text.length (UUID.toText UUID.nil)
    let allSessionIds = map (.id) allSessions
    text
        |> Text.chunksOf uuidCharCount
        |> mapMaybe UUID.fromText
        |> filter (\id -> id `elem` allSessionIds)
        |> pure

gcSessions :: IORef LocalRefreshServer -> IO ()
gcSessions localRefreshServer = do
    now <- getCurrentTime
    modifyIORef' localRefreshServer (\localRefreshServer -> localRefreshServer { sessions = filter (not . isSessionExpired now) localRefreshServer.sessions } :: LocalRefreshServer)

isSessionExpired :: UTCTime -> LocalRefreshSession -> Bool
isSessionExpired now LocalRefreshSession { lastHeartbeat } = (now `diffUTCTime` lastHeartbeat) > (secondsToNominalDiffTime 60)

registerNotificationTrigger :: (?modelContext :: ModelContext) => IORef (Set ByteString) -> IORef LocalRefreshServer -> IO ()
registerNotificationTrigger touchedTablesVar localRefreshServer = do
    touchedTables <- Set.toList <$> readIORef touchedTablesVar
    subscribedTables <- (.subscribedTables) <$> (localRefreshServer |> readIORef)

    let subscriptionRequired = touchedTables |> filter (\table -> subscribedTables |> Set.notMember table)
    modifyIORef' localRefreshServer (\server -> server { subscribedTables = server.subscribedTables <> Set.fromList subscriptionRequired } :: LocalRefreshServer)

    pgListener <- (.pgListener) <$> readIORef localRefreshServer
    subscriptions <-  subscriptionRequired |> mapM (\table -> do
        let createTriggerSql = notificationTrigger table

        -- We need to add the trigger from the main IHP database role other we will get this error:
        -- ERROR:  permission denied for schema public
        withRowLevelSecurityDisabled do
            sqlExec createTriggerSql ()

        pgListener |> PGListener.subscribe (channelName table) \notification -> do
                sessions <- (.sessions) <$> readIORef localRefreshServer
                sessions
                    |> filter (\session -> table `Set.member` session.tables)
                    |> map (\session -> session.event)
                    |> mapM (\event -> MVar.tryPutMVar event ())
                pure ())
    modifyIORef' localRefreshServer (\s -> s { subscriptions = s.subscriptions <> subscriptions } :: LocalRefreshServer)
    pure ()


channelName :: ByteString -> ByteString
channelName tableName = "lr_did_change_" <> tableName

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
        functionName = "lr_notify_did_change_" <> tableName
        insertTriggerName = "lr_did_insert_" <> tableName
        updateTriggerName = "lr_did_update_" <> tableName
        deleteTriggerName = "lr_did_delete_" <> tableName