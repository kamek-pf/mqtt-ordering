module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, unless, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Exit (exitFailure, exitSuccess)

import Data.Binary (decode, encode)
import Data.ByteString.Lazy (ByteString, putStr)
import Network.MQTT.Client
import Network.URI (parseURI)

-- callback = SimpleCallback 
callback = OrderedCallback

messageCount :: Int
-- messageCount = 100_000
messageCount = 1_000_000

qos :: QoS
qos = QoS0

main :: IO ()
main = do
    let (Just uri) = parseURI "mqtt://localhost:1883"
    countRef <- newIORef @Int 0
    errSem <- newIORef Nothing
    client <- connectURI mqttConfig {_msgCB = callback (readerCallback errSem countRef)} uri
    subscribe client [("test/topic", subOptions {_subQoS = qos})] []
    forkIO $ publishMessages client
    waitForMessages client errSem countRef

publishMessages :: MQTTClient -> IO ()
publishMessages client = doPublish 0
  where
    doPublish num
        | num <= messageCount = do
            publishq client "test/topic" (encode num) False qos []
            doPublish $ num + 1
        | otherwise = putStrLn "done publishing"

readerCallback :: IORef (Maybe ()) -> IORef Int -> MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
readerCallback errSem countRef client _ msg _ = do
    oldCount <- readIORef countRef
    let newCount = decode msg
    when (newCount >= 1 && newCount /= oldCount + 1) $ onErr oldCount newCount
    writeIORef countRef newCount
  where
    onErr old new = do 
        putStrLn $ show old <> " -> " <> show new
        writeIORef errSem (Just ())

waitForMessages :: MQTTClient -> IORef (Maybe ()) -> IORef Int -> IO ()
waitForMessages client errSem countRef = do
    threadDelay 100_000
    current <- readIORef countRef
    didFail <- readIORef errSem
    let progress = current * 100 `div` messageCount
    putStrLn $ show progress <> "% ..."
    case (progress, didFail) of
        (100, _) -> do
            normalDisconnect client
            waitForClient client
            putStrLn "all messages were processed in order"
        (_, Just ()) -> error "messages unordered"
        _ -> waitForMessages client errSem countRef
