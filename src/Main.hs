{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (writeChan, readChan, newChan)
import Control.Monad (forever)
import System.IO

import qualified Data.ByteString as BS
import qualified AgarFeed as AF

rawLogger raw_chan = do
        log <- openBinaryFile "output" WriteMode
        forever $ do
            dat <- readChan raw_chan
            BS.hPutStrLn log dat

messageLogger chan = do
        forever $ do
            m <- readChan chan
            print m

main = do
        raw_chan <- newChan
        chan     <- newChan

        forkIO $ rawLogger raw_chan
        forkIO $ messageLogger chan

        AF.startFeed [raw_chan] [chan] "US-Atlanta"
