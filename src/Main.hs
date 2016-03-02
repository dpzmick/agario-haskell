{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Monad (forever)
import System.IO

import qualified Data.ByteString.Char8 as BS8
import qualified AgarFeed as AF

rawLogger raw_chan = do
        log <- openBinaryFile "output" WriteMode
        forever $ do
            dat <- readChan raw_chan
            BS8.hPutStrLn log dat

messageLogger chan = do
        forever $ do
            m <- readChan chan
            print m

main :: IO ()
main = do
        raw_chan <- newChan
        chan     <- newChan

        _ <- forkIO $ rawLogger raw_chan
        _ <- forkIO $ messageLogger chan

        AF.startFeed [raw_chan] [chan] "US-Atlanta"
