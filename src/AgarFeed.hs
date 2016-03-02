{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module AgarFeed where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Functor
import Data.Word
import Data.Char
import Data.Bits

import System.IO
import Control.Concurrent.Chan (writeChan)

import Control.Monad (forever)

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as BSL

import qualified Network.WebSockets as WS

import AgarServerInfo

data Point = Point {x :: !Float, y :: !Float} deriving(Show)
data PointWord32 = PointWord32 {xi :: !Word32, yi :: !Word32} deriving(Show)

data Eat = Eat { eater_id :: Word32, victim_id :: Word32 } deriving(Show)

data Color = Color Word8 Word8 Word8 deriving(Show)

data Removal = Removal deriving(Show)

data Update = Update { player_id :: Word32
                     , loc :: PointWord32
                     , radius :: Word16
                     , color :: Color
                     , name :: String
                     , virus :: Bool
                     } deriving(Show)

data IncomingMessage =
          ViewUpdate !Point !Float
        | ResetAll
        | Reset
        | OwnsBlob !Word32
        | FFALeaderboard ![(Word32,String)]
        | GameAreaSize Point Point
        | WorldUpdate [Eat] [Update] [Removal]
        | Error
        deriving(Show)

instance Binary IncomingMessage where
        put _ = error "should not be trying to deserialize"

        get = do
            t <- getWord8

            case t of
                -- big hairy mess
                16 -> do
                    cnt_eats <- getWord16le
                    eats <- readEats [] cnt_eats
                    updates <- readUpdates []
                    return $ WorldUpdate eats updates []

                17 -> do
                    x <- getFloat32le
                    y <- getFloat32le
                    zoom <- getFloat32le
                    return $ ViewUpdate (Point x y) zoom

                18 -> return $ ResetAll

                20 -> return $ Reset

                32 -> do
                    blob_id <- getWord32le
                    return $ OwnsBlob blob_id

                49 -> do
                    cnt <- getWord32le
                    vs  <- getEntries [] cnt
                    return $ FFALeaderboard vs

                64 -> do
                    min_x <- getFloat32le
                    min_y <- getFloat32le
                    max_x <- getFloat32le
                    max_y <- getFloat32le
                    return $ (GameAreaSize (Point min_x min_y) (Point max_x max_y))

                _ -> return Error

getEntries acc 0 = return $ acc
getEntries acc r = do
        id <- getWord32le
        s  <- popStr16 []
        getEntries (acc ++ [(id,s)]) (r - 1)


readEats acc 0 = return $ acc
readEats acc r = do
        eater <- getWord32le
        eaten <- getWord32le
        readEats ((Eat eater eaten):acc) (r - 1)

readUpdates acc = do
        id <- getWord32le
        if id == 0
            then return $ acc
            else do
                x <- getWord32le
                y <- getWord32le
                rad <- getWord16le

                -- colors
                r <- getWord8
                g <- getWord8
                b <- getWord8

                -- flag stuff
                flags <- getWord8
                if flags .&. 0x02 /= 0
                    then do
                        amount <- getWord32le
                        skipBytes (fromIntegral amount)
                    else return ()
                if flags .&. 0x04 /= 0
                    then do
                        ignored <- popStr8 []
                        return ()
                    else return ()

                name <- popStr16 []
                let up = Update { player_id = id
                                , loc       = (PointWord32 x y)
                                , color     = (Color r g b)
                                , radius    = rad
                                , name      = name
                                , virus     = flags .&. 0x01 /= 0}
                readUpdates (up:acc)

skipBytes 0 = return ()
skipBytes a = do
        s <- getWord8
        skip (a - 1)

popStr8 acc = do
        r <- getWord8
        if r == 0
            then return acc
            else popStr8 (acc ++ [(chr (fromIntegral r :: Int))])

popStr16 acc = do
        r <- getWord16le
        if r == 0
            then return acc
            else popStr16 (acc ++ [(chr (fromIntegral r :: Int))])

data OutgoingMessage =
          Subscribe1   !Word32
        | Subscribe2   !Word32
        | Spectate
        | ConnectTok   !String -- word 8 array
        | NickAndSpawn !String -- word 8 array
        | SetDirection !Word32 !Word32
        | Split
        | EjectMass
        deriving(Show)

instance Binary OutgoingMessage where
        put (Subscribe1 arg) = do
            putWord8 254
            putWord32le arg

        put (Subscribe2 arg) = do
            putWord8 255
            putWord32le arg

        put (Spectate) = putWord8 1

        get = error "should not try to deserialize"

-- message types
subs1    = Subscribe1 4
subs2    = Subscribe2 154669603

client token raw_clients clients conn = do
        putStrLn "Connected, now subscribing"
        WS.sendBinaryData conn (encode subs1)
        WS.sendBinaryData conn (encode subs2)
        WS.sendBinaryData conn (encode $ Spectate)

        forever $ do
            dat <- WS.receiveDataMessage conn
            case dat of
                WS.Text bs -> print "what"
                WS.Binary bs -> do
                    mapM_ (\c -> writeChan c $ BSL.toStrict bs) raw_clients
                    mapM_ (\c -> writeChan c (decode bs :: IncomingMessage)) clients

startFeed raw_clients clients region = do
        info <- getServerInfo region 154669603

        case info of
            ServerInfo url port token ->
                WS.runClient (T.unpack url) port "/" $ client token raw_clients clients
