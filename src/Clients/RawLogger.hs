module Clients.RawLogger where

import Control.Monad (forever)
import Control.Concurrent.Chan
import System.IO

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8

-- a client which reads from a raw channel and dumps the output to disk
client :: Chan BS.ByteString -> IO ()
client raw_chan = do
        output <- openBinaryFile "output" WriteMode
        forever $ do
            dat <- readChan raw_chan
            BS8.hPutStrLn output dat
