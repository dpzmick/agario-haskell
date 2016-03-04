{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

-- this module abuses strings in every way imaginable

module AgarFeed where

----------------------------------------------------------------------------
-- cool binary stuff
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.IEEE754

-- enhanced builtins
import Data.Char
import Data.Bits
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL

-- server things
import Control.Concurrent      (forkIO)
import Control.Concurrent.Chan (writeChan, readChan, Chan)
import Control.Monad           (forever)

import qualified Network.WebSockets as WS
----------------------------------------------------------------------------

data LeaderboardEntry = LeaderboardEntry !Word32 !String deriving(Show)

data Point a = Point !(a, a) deriving(Show, Eq)

data Eat = Eat { eater_id :: BlobID, victim_id :: BlobID } deriving(Show)

data Color = Color Word8 Word8 Word8 deriving(Show, Eq)

data Removal = Removal deriving(Show)

type BlobID = Word32

data Update = Update { player_id :: BlobID
                     , loc       :: Point Word32
                     , radius    :: Word16
                     , color     :: Color
                     , name      :: String
                     , virus     :: Bool
                     } deriving(Show, Eq)

data IncomingMessage =
          ViewUpdate !(Point Float) !Float
        | ResetAll
        | Reset
        | OwnsBlob !BlobID
        | FFALeaderboard ![LeaderboardEntry]
        -- TODO something is wrong with GameAreaSize on a real server
        | GameAreaSize (Point Double) (Point Double)
        | WorldUpdate [Eat] [Update] [Removal]
        | Error -- catch all
        deriving(Show)

data OutgoingMessage =
          Subscribe1   !Word32
        | Subscribe2   !Word32
        | ConnectTok   !String
        | NickAndSpawn !String
        | Spectate
        -- actions TODO fill in the rest
        | SetDirection !(Point Word32)
        | Split
        | EjectMass
        deriving(Show)

type IQueue = Chan IncomingMessage
type OQueue = Chan OutgoingMessage

----------------------------------------------------------------------------
-- here be dragons

instance Binary IncomingMessage where
        put _ = error "should not be trying to deserialize"

        get = do
            t <- getWord8

            case t of
                16 -> do
                    cnt_eats <- getWord16le
                    eats <- readEats [] cnt_eats
                    updates <- readUpdates []
                    return $ WorldUpdate eats updates []

                17 -> do
                    x <- getFloat32le
                    y <- getFloat32le
                    zoom <- getFloat32le
                    return $ ViewUpdate (Point (x, y)) zoom

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
                    min_x <- getFloat64le
                    min_y <- getFloat64le
                    max_x <- getFloat64le
                    max_y <- getFloat64le
                    return $ (GameAreaSize (Point (min_x,min_y)) (Point (max_x, max_y)))

                _ -> return Error

getEntries :: [LeaderboardEntry] -> Word32 -> Get [LeaderboardEntry]
getEntries acc 0 = return $ acc
getEntries acc r = do
        eid <- getWord32le
        s  <- popStr16 []
        getEntries (acc ++ [LeaderboardEntry eid s]) (r - 1)

readEats :: [Eat] -> Word16 -> Get [Eat]
readEats acc 0 = return $ acc
readEats acc r = do
        eater <- getWord32le
        eaten <- getWord32le
        readEats ((Eat eater eaten):acc) (r - 1)

readUpdates :: [Update] -> Get [Update]
readUpdates acc = do
        pid <- getWord32le
        if pid == 0
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
                        _ <- popStr8 [] -- ignore this
                        return ()
                    else return ()

                pname <- popStr16 []
                let up = Update { player_id = pid
                                , loc       = (Point (x, y))
                                , color     = (Color r g b)
                                , radius    = rad
                                , name      = pname
                                , virus     = flags .&. 0x01 /= 0}
                readUpdates (up:acc)
                -- TODO read removals

instance Binary OutgoingMessage where
        put (Subscribe1 arg) = do
            putWord8 254
            putWord32le arg

        put (Subscribe2 arg) = do
            putWord8 255
            putWord32le arg

        put (Spectate) = putWord8 1

        put (ConnectTok token_string) = do
            putWord8 80
            putStr8End token_string

        put (NickAndSpawn pname) = do
            putWord8 0
            putStr16End pname

        put (SetDirection (Point (x,y))) = do
            putWord8 16
            putWord32le x
            putWord32le y
            putWord32le 123 -- garbage

        put (Split) = error "TODO not implemented"
        put (EjectMass) = error "TODO not implemented"

        get = error "should not try to deserialize"

----------------------------------------------------------------------------
-- various helpers
-- the string functions are hacky

-- both popStr functions pop a string until \0

popStr8 :: [Char] -> Get [Char]
popStr8 acc = do
        r <- getWord8
        if r == 0
            then return acc
            else popStr8 (acc ++ [(chr (fromIntegral r :: Int))])

popStr16 :: [Char] -> Get [Char]
popStr16 acc = do
        r <- getWord16le
        if r == 0
            then return acc
            else popStr16 (acc ++ [(chr (fromIntegral r :: Int))])

-- both putStr*End functions right the string, but do not write a null
-- terminator

putStr8End :: [Char] -> Put
putStr8End [] = return ()
putStr8End (c:cs) = do
        putWord8 $ fromIntegral $ ord c
        putStr8End cs

putStr16End :: [Char] -> Put
putStr16End [] = return ()
putStr16End (c:cs) = do
        putWord16le $ fromIntegral $ ord c
        putStr16End cs

skipBytes :: Int -> Get ()
skipBytes 0 = return ()
skipBytes a = do
        _ <- getWord8
        skip (a - 1)

----------------------------------------------------------------------------
-- client/server stuff

feed :: FeedConfig -> WS.Connection -> IO ()
feed fc conn = do
        putStrLn "Feed connected"

        _ <- forkIO $ forever $ do
            command <- readChan (commandChan fc)
            WS.sendBinaryData conn (encode command)

        forever $ do
            dat <- WS.receiveDataMessage conn

            case dat of

                WS.Binary bs -> do
                    -- send the message to every raw client
                    mapM_ (\c -> writeChan c $ BSL.toStrict bs) (rawClients fc)

                    -- send the message to ever message client
                    mapM_ (\c -> writeChan c (decode bs :: IncomingMessage)) (messageClients fc)

                _ -> do
                    putStrLn "invalid message received"
                    return ()

-- rawClients get raw binary data
-- messageClients get IncomingMessages
-- commandChan takes OutgoingMessages and sends them to agar
--
-- there can be multiple clients, but all clients must share the same
-- commandChan
data FeedConfig = FeedConfig { rawClients     :: [Chan BS.ByteString]
                             , messageClients :: [Chan IncomingMessage]
                             , commandChan    :: Chan OutgoingMessage
                             , url            :: String
                             , port           :: Int
                             }

startFeed :: FeedConfig -> IO ()
startFeed fc = WS.runClient (url fc) (port fc) "/" $ feed fc
