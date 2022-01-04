module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, unless, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Exit (exitFailure, exitSuccess)

import Data.Binary (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Network.MQTT.Client
import Network.URI (parseURI)
import System.Random (randomRIO)

messageCount :: Int
messageCount = 1_000_000

qos :: QoS
qos = QoS0

main :: IO ()
main = do
    let (Just uri) = parseURI "mqtt://localhost:1883"
    countRef <- newIORef @Int 0
    errSem <- newIORef Nothing
    client <- connectURI mqttConfig {_msgCB = SimpleCallback (readerCallback errSem countRef)} uri
    subscribe client [("test/topic", subOptions {_subQoS = qos})] []
    forkIO $ publishMessages client
    waitForMessages client errSem countRef

publishMessages :: MQTTClient -> IO ()
publishMessages client = doPublish 0
  where
    doPublish !num
        | num <= messageCount = do
            publishq client "test/topic" (encode num) False qos []
            doPublish $ num + 1
        | otherwise = pure ()

readerCallback :: IORef (Maybe ()) -> IORef Int -> MQTTClient -> Topic -> ByteString -> [Property] -> IO ()
readerCallback errSem countRef client _ msg _ = do
    -- Add some random jitter
    delay <- randomRIO (5, 1000)
    threadDelay delay
    oldCount <- readIORef countRef
    let newCount = decode msg
    when (newCount >= 1 && newCount < oldCount + 1) $ writeIORef errSem (Just ()) -- Allow duplicates
    -- when (newCount >= 1 && newCount <= oldCount + 1) $ writeIORef errSem (Just ()) -- Forbid duplicates
    writeIORef countRef newCount

waitForMessages :: MQTTClient -> IORef (Maybe ()) -> IORef Int -> IO ()
waitForMessages !client errSem !countRef = do
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
