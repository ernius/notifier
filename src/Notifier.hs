{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}

module Notifier
    ( startNotifier,
      notifierApp,
      newServerState,
      notifierAPI,
      notifierHttpAPI,
      SubscriptionInput(..),
      Event(..)
    ) where

import Prelude              hiding   (concat)

import Control.Monad                 (forever)
import Control.Monad.IO.Class        (MonadIO (..))
import Control.Exception             (finally, catch)
import Control.Concurrent.Async      (mapConcurrently_)
import Control.Concurrent            (MVar, newMVar, modifyMVar_, readMVar)
import Data.Aeson                    (FromJSON, ToJSON, decode, encode)
import Data.Text                     (Text, concat, pack)
import Data.List                     (groupBy)

import GHC.Generics                  (Generic)
import Network.Wai                   (Application)
import Network.Wai.Handler.Warp      (run)
import Servant                       ((:<|>) (..), (:>), Proxy (..), Server, serve, Get, Post, Handler, PlainText, JSON, ReqBody)
import Servant.API.WebSocket         (WebSocket)

import qualified Data.HashMap.Strict        as M
import qualified Data.Text.Lazy             as T (toStrict)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy.Builder     as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.IO               as T
import qualified Network.WebSockets         as WS

type Timestamp = Int

type Clients = M.HashMap Text (WS.Connection, Timestamp)

data DropUser = DropUser
  {
    drop_access_id :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON DropUser
instance ToJSON   DropUser

data SubscriptionInput = SubscriptionInput
  {
    access_id :: Text
  , timestamp :: Timestamp
  } deriving (Eq, Show, Generic)

instance FromJSON SubscriptionInput
instance ToJSON   SubscriptionInput

data Event = Event 
  {  accessid :: Text
  ,  created_at :: Timestamp
  ,  payload :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Event
instance ToJSON   Event

-- | Clients handling abstrated =============================

-- | Create a new, initial state:
newServerState :: Clients
newServerState = M.empty

-- | Get client connection
getClient :: Text -> Clients -> Maybe (WS.Connection, Timestamp)
getClient = M.lookup

-- | Add a client (this does not check if the client already exists, you should do
-- | this yourself using `clientExists`):
addClient :: Text -> (WS.Connection, Timestamp)  -> Clients -> Clients
addClient = M.insert

-- | Remove a client:
removeClient :: Text -> Clients -> Clients
removeClient = M.delete

-- | Servant API =======================================================================
type NotifierHttpAPI =
           "clients"     :> Get '[PlainText] Text -- testing endpoint 
      :<|> "push_events" :> ReqBody '[JSON] [Event]  :> Post '[PlainText] Text
      :<|> "drop_user"   :> ReqBody '[JSON] DropUser :> Post '[PlainText] Text

type NotifierWebsocketAPI =      
           "subscribe_to_events" :> WebSocket

type NotifierAPI = NotifierHttpAPI :<|> NotifierWebsocketAPI

server :: MVar Clients -> Server NotifierAPI
server state = (    clientsHttp       state
               :<|> pushEvents        state
               :<|> dropClient        state)
               :<|> subscribeToEvents state

notifierApp :: MVar Clients -> Application
notifierApp = serve notifierAPI . server

notifierHttpAPI :: Proxy NotifierHttpAPI
notifierHttpAPI = Proxy

notifierAPI :: Proxy NotifierAPI
notifierAPI = Proxy

startNotifier :: IO ()
startNotifier = do
  putStrLn "Starting Notifier Server on http://localhost:8080"
  state <- newMVar newServerState
  run 8080 $ notifierApp state

clientsHttp :: MVar Clients -> Handler Text
clientsHttp state = liftIO $ do
  clients <- readMVar state
  return $ "Connected clients:" <> (concat $ map (<> " ") $ M.keys clients)

-- | Drop User
-- | Example: curl -H "Content-Type: application/json" -X POST -d '{"drop_access_id":"client1"}' localhost:8080/drop_user
dropClient :: MVar Clients -> DropUser -> Handler Text
dropClient state (DropUser { drop_access_id = accessid }) = liftIO $ do
  clients <- readMVar state
  case getClient accessid clients of
    Nothing        -> return ()
    Just (conn, _) -> do
      T.putStrLn $ "Droped client: " <> accessid
      WS.sendClose conn ("close" :: Text)
  return "true"

-- | Push Events
-- | Example: curl -H "Content-Type: application/json" -X POST -d '[{"accessid":"ernesto", "created_at": 10, "payload" : "contenido  otroernesto"},{"accessid":"ernesto2", "created_at": 10, "payload" : "contenido otro ernesto2"}]' localhost:8080/push_events
pushEvents :: MVar Clients -> [Event] -> Handler Text
pushEvents state events = liftIO $ do
  T.putStrLn $ "Pushed events: " <> pack (show events)
  clients <- readMVar state
  mapConcurrently_ (\ evs@(e:_) -> case getClient (accessid e) clients of -- TODO: update client timestamp ? With max. timestamp in client's messages 
                                     Nothing            -> return ()
                                     Just (conn, tstmp) -> sendClient conn (filter (\ e -> created_at e >= tstmp) evs))
                   (groupBy (\ e1 e2 -> accessid e1 == accessid e2) events) -- TODO: this only group contiguous events of the same client !!
  return $ "true"

-- | Client subscription
-- | Example: > wscat -c http://localhost:8080/subscribe_to_events
-- |          connected (press CTRL+C to quit)
-- |          > { "access_id" : "client1", "timestamp" : 2000 }
subscribeToEvents :: MonadIO m => MVar Clients -> WS.Connection -> m ()
subscribeToEvents state conn = liftIO . WS.withPingThread conn 30 (return ()) $ do
  msg <- WS.receiveData conn
  case decode msg :: Maybe SubscriptionInput of
    Nothing -> WS.sendTextData conn ("Wrong JSON announcement" :: Text)
    Just (SubscriptionInput{ access_id = user, timestamp = tstmp })  -> do
      -- TODO: missing access_id validation against ON-RAMP server
      T.putStrLn $ "Subscribed client: " <> user 
      flip finally (disconnectClient state user) $ do
        modifyMVar_ state $ \clients -> return $ addClient user (conn, tstmp)  clients
        talkClient conn

talkClient :: WS.Connection -> IO ()
talkClient conn = forever (WS.receiveData conn :: IO Text) `catch` closeConnection -- TODO: messages from clients are meaningless?
  where
    closeConnection (WS.CloseRequest _ _) = T.putStrLn "Connection closed in a good way"
    closeConnection WS.ConnectionClosed   = T.putStrLn "Connection closed in an unpolite way"
    closeConnection e                     = T.putStrLn $ "Something strange happen" <> pack (show e)

sendClient :: WS.Connection -> [Event] -> IO ()
sendClient _    []     = return ()
sendClient conn events = WS.sendTextData conn (encode events)

-- | Remove client and return new state
disconnectClient :: MonadIO m => MVar Clients -> Text -> m ()
disconnectClient state client = 
  liftIO . modifyMVar_ state $ \s -> return $ removeClient client s
  
