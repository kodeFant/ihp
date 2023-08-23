module IHP.ApplicationContext where

import IHP.Prelude
import Network.Wai.Session (Session)
import qualified Data.Vault.Lazy as Vault
import IHP.AutoRefresh.Types (AutoRefreshServer)
import IHP.LocalRefresh.Types (LocalRefreshServer)
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.PGListener (PGListener)

data ApplicationContext = ApplicationContext
    { modelContext :: !ModelContext
    , session :: !(Vault.Key (Session IO ByteString ByteString))
    , autoRefreshServer :: !(IORef AutoRefreshServer)
    , localRefreshServer :: !(IORef LocalRefreshServer)
    , frameworkConfig :: !FrameworkConfig
    , pgListener :: PGListener
    }
