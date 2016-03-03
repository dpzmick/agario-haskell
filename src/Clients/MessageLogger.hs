module Clients.MessageLogger where

import Control.Monad (forever)
import Control.Concurrent.Chan

import AgarFeed

-- a client which reads from a message channel and logs sometimes
client :: Chan IncomingMessage -> IO ()
client chan = do
        forever $ do
            m <- readChan chan
            case m of
                FFALeaderboard _ -> print m
                _ -> return ()

