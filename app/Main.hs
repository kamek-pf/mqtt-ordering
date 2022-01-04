module Main where

import Control.Concurrent (forkIO)
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Data.Binary (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Network.MQTT.Client
import Network.URI (parseURI)

main :: IO ()
main = do
    let (Just uri) = parseURI "mqtt://localhost:1883"
    countRef <- newIORef @Int 0
    client <- connectURI mqttConfig {_msgCB = SimpleCallback (readerCallback countRef)} uri
    subscribe client [("test/topic", subOptions)] []
    forkIO $ publishMessages client
    waitForClient client
    pure ()

messageCount :: Int
messageCount = 1_000_000

publishMessages :: MQTTClient -> IO ()
publishMessages client = doPublish 0
  where
    doPublish !num
        | num <= messageCount = do
            publish client "test/topic" (encode num) False
            doPublish $ num + 1
        | otherwise = pure ()

readerCallback :: IORef Int -> MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
readerCallback countRef client _ msg _ = do
    oldCount <- readIORef countRef
    let newCount = decode msg
    -- The print here introduces a small variable delay that should be enough to trigger the issue
    print $ show oldCount <> " -> " <> show newCount
    when (newCount >= 1 && oldCount + 1 /= newCount) $ error "messages unordered"
    when (newCount == messageCount) $ normalDisconnect client
    writeIORef countRef newCount
