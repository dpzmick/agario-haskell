module Clients.MessageLogger where

import Control.Monad (forever)
import Control.Concurrent.Chan

import AgarFeed

import System.IO

-- a client which reads from a message channel and logs sometimes
client :: Chan IncomingMessage -> IO ()
client chan = do
        handle <- openFile "output-messages" WriteMode
        forever $ do
            m <- readChan chan
            hPutStrLn handle (show m)

