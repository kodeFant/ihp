module IHP.LocalRefresh.Types where


import IHP.Prelude
import IHP.Controller.RequestContext
import Control.Concurrent.MVar (MVar)
import qualified IHP.PGListener as PGListener


data LocalRefreshState = LocalRefreshDisabled | LocalRefreshEnabled { sessionId :: !UUID }

data LocalRefreshSession = LocalRefreshSession
        { id :: !UUID
        -- | MVar that is filled whenever some table changed
        , event :: !(MVar ())
        -- | All tables this auto refresh session watches
        , tables :: !(Set ByteString)
        -- | The last rendered SSE response of this action
        , lastResponse :: !LByteString
        -- | Keep track of the last ping to this session to close it after too much time has passed without anything happening
        , lastHeartbeat :: !UTCTime
        }

data LocalRefreshServer = LocalRefreshServer
        { subscriptions :: [PGListener.Subscription]
        , sessions :: ![LocalRefreshSession]
        , subscribedTables :: !(Set ByteString)
        , pgListener :: PGListener.PGListener
        }