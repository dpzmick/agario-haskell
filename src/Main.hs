module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan

-- import some clients
import qualified Clients.RawLogger     as RawLog
import qualified Clients.MessageLogger as MessLog
import qualified Clients.CircleBot     as CircleBot

import AgarFeed

-- there are different versions
-- writeChan commands Subscribe1 4
-- writeChan commands Subscribe2 154669603
-- send the initial connection messages
-- TODO do we want to spawn right away?
subscribeWithName :: String -> Chan OutgoingMessage -> IO ()
subscribeWithName player_name commands = do
        writeChan commands $ Subscribe1 5
        writeChan commands $ Subscribe2 2200049715
        -- TODO writeChan commands $ ConnectTok token
        writeChan commands $ NickAndSpawn player_name

main :: IO ()
main = do
        -- create a channel (like a queue, to send commands to the server)
        commands <- newChan

        -- create a client which dumps the raw data to disk
        raw_chan <- newChan
        _ <- forkIO $ RawLog.client raw_chan

        -- create a client that does some logging
        loggerChan <- newChan
        _ <- forkIO $ MessLog.client loggerChan

        -- create a client that steers us in circles
        circleChan <- newChan
        _ <- forkIO $ CircleBot.client circleChan commands

        -- TODO to connect to the real agar severs, need to use ServerInfo

        -- queue up the subscribe message
        subscribeWithName "dpzmick" commands

        -- start the feed
        let fc = FeedConfig { rawClients     = [raw_chan]
                            , messageClients = [loggerChan, circleChan]
                            , commandChan    = commands
                            , url            = "127.0.0.1"
                            , port           = 8889 }
        startFeed fc
