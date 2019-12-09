{-# LANGUAGE TypeFamilies, DataKinds, DeriveGeneric, TypeOperators, OverloadedStrings, ScopedTypeVariables #-}



import           Control.Monad                       (forever, unless)
import           Control.Monad.STM                   (atomically)
import           Control.Concurrent                  (forkIO, killThread, newMVar, threadDelay)
import           Control.Concurrent.Chan
import           Control.Exception                   (bracket, catch, SomeException, handle)
import           Control.Monad.IO.Class              (liftIO)

import           Data.Text                           (Text)
import qualified Data.Text.IO                 as T
import           Data.Aeson                          (decode, encode)
import           GHC.Generics

import           Network.HTTP.Client          hiding (Proxy, responseBody)
import           Network.Socket                      (withSocketsDo)
import           Network.Wai.Handler.Warp            (run)
import qualified Network.WebSockets           as WS

import           Servant
import           Servant.Client
import           Servant.Server

import           Test.Hspec
import           Test.HUnit                          ((@=?))

import           Notifier

testHost :: String
testHost = "localhost"

testPort :: Int
testPort = 8080

testServerUrl :: String
testServerUrl = "http://" ++ testHost ++ ":" ++ show testPort

withNotifierApp :: IO () -> IO ()
withNotifierApp action = do
  bracket
    (forkIO startNotifier)
    killThread
    (const $ do
        waitSome -- wait for server to be up in previous fork
        action)

businessLogicSpec :: Spec
businessLogicSpec = around_ withNotifierApp $ do
  let clients :<|> push :<|> drop = client notifierHttpAPI
  baseUrl <- runIO $ parseBaseUrl $ testServerUrl
  manager <- runIO $ newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager baseUrl
  ch <- runIO $ newChan -- to sync push and clients subscription threads 
  
  describe "get connected clients" $ do
    it "should get an empty list of clients" $ do    
      result <- runClientM clients clientEnv
      result `shouldBe` Right "Connected clients:"

  describe "test websocket subscribe_to_events" $ do
    it "subscribe 2 clients and push 3 events" $ do
      forkIO $ do
        readChan ch -- wait the two websockets clients are subscribed
        readChan ch
        Right "true" <- runClientM (push [event1, event2, event3, event4]) clientEnv
        return ()
      withSocketsDo $ do
        forkIO $ subscribeWebsocket (subscribeApp ch subscriptionInputTest1 [event1]) `shouldReturn` True
        subscribeWebsocket (subscribeApp ch subscriptionInputTest2 [event2]) `shouldReturn` True

-- { "access_id" : "client1", "timestamp" : 2000 }
subscriptionInputTest1 :: SubscriptionInput
subscriptionInputTest1 = SubscriptionInput {
    access_id = "client1" :: Text
  , timestamp = 1000
  }

subscriptionInputTest2 :: SubscriptionInput
subscriptionInputTest2 = subscriptionInputTest1 { access_id = "client2" :: Text }

event1 :: Event
event1 = Event {
    accessid   = "client1" :: Text
  , created_at = 1000
  , payload    = "client1 event" :: Text
  }

event2 :: Event
event2 = Event {
    accessid   = "client2" :: Text
  , created_at = 3000
  , payload    = "client2 event" :: Text
  }

event3 :: Event
event3 = Event {
    accessid   = "client3" :: Text
  , created_at = 4000
  , payload    = "client3 event" :: Text
  }

-- old timestamp
event4 :: Event
event4 = Event {
    accessid   = "client1" :: Text
  , created_at = 500
  , payload    = "old client1 event" :: Text
  }

subscribeWebsocket :: WS.ClientApp Bool -> IO Bool
subscribeWebsocket = WS.runClient "127.0.0.1" testPort "/subscribe_to_events"

subscribeApp :: Chan () -> SubscriptionInput -> [Event] -> WS.ClientApp Bool
subscribeApp ch subs events conn = do
  WS.sendTextData conn (encode subs)
  writeChan ch () -- sync with push event thread
  msg <- WS.receiveData conn
  WS.sendCloseCode conn 1000 ("" :: Text)
  expectCloseException conn "close"
  return $ (decode msg) == Just events

waitSome :: IO ()
waitSome = threadDelay $ 200 * 1000

expectCloseException :: WS.Connection -> Text -> IO ()
expectCloseException conn msg = act `catch` handler
    where
        act = WS.receiveDataMessage conn >>= \ msg -> error $ "Expecting CloseRequest exception, but received:" ++ show msg
        handler (WS.CloseRequest i msg') = do
            i @=? 1000
--            decode utf8 msg' @=? msg
        handler WS.ConnectionClosed     = error "Unexpected connection closed"
        handler (WS.ParseException _)   = error "Unexpected parse exception"
        handler (WS.UnicodeException _) = error "Unexpected unicode exception"        

spec :: Spec
spec = businessLogicSpec

main :: IO ()
main = hspec spec  
    

