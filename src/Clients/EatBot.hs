-- just tries to eat stuff
-- doesn't try to not die

module Clients.EatBot (client) where

import Control.Monad (forever)
import Control.Concurrent.Chan

import Data.Word
import Data.List (sortBy)
import Safe (headMay)

import AgarFeed

data GameInfo = GameInfo { myLocation  :: Point Word32
                         , myID        :: BlobID
                         , targetPoint :: Maybe (Point Word32)
                         , incoming    :: IQueue
                         , outgoing    :: OQueue

                         -- the game doesn't send you updates for things
                         -- you've already seen, unless they change.
                         -- cache the updates
                         , seen        :: [Update]
                         }

-- much of this logic may be a good candidate for a generic "bootstrap"
-- function

-- looks for an update giving your current location in the list of updates
-- given
getMyLocation :: BlobID -> [Update] -> Maybe (Point Word32)
getMyLocation bid updates = headMay $ map loc $ filter f updates
    where f o = (player_id o) == bid

-- gets the closest point
getClosest :: Point Word32 -> [Point Word32] -> Maybe (Point Word32)
getClosest _ [] = Nothing
getClosest location locs = headMay sorted
    where sorted = sortBy (distCmp location) locs

distCmp :: Point Word32 -> Point Word32 -> Point Word32 -> Ordering
distCmp origin p1 p2
    | (distance p1 origin) < (distance origin p2) = LT
    | (distance p1 origin) > (distance origin p2) = GT
    | True                                        = EQ

distance :: Point Word32 -> Point Word32 -> Double
distance (Point (x1,y1)) (Point (x2, y2)) = sqrt (a + b)
    where a = ((fromIntegral x2) - (fromIntegral x1))**2.0
          b = ((fromIntegral y2) - (fromIntegral y1))**2.0

----------------------------------------------------------------------------

client :: IQueue -> OQueue -> IO ()
client incoming outgoing = forever $ do
        -- need to get our own id, then start playing
        print "waiting for id"
        message <- readChan incoming

        -- we should cache updates we see here
        case message of
            OwnsBlob blob_id        -> inner blob_id
            _                       -> return ()

    where
        tryToStart blob_id updates = do
            print $ getMyLocation blob_id updates

            -- we should cache updates here
            case getMyLocation blob_id updates of
                Just p  -> go GameInfo { myLocation  = p
                                       , myID        = blob_id
                                       , targetPoint = Nothing
                                       , incoming    = incoming
                                       , outgoing    = outgoing
                                       , seen        = updates }
                Nothing -> return ()

        inner blob_id = forever $ do
          -- start moving a bit, to trigger a world update in case we missed
          -- it waiting for our id
          writeChan outgoing (SetDirection (Point (0,0)))

          -- get our location
          message <- readChan incoming
          case message of
              WorldUpdate _ updates _ -> tryToStart blob_id updates
              _ -> return ()

----------------------------------------------------------------------------

-- removes updates that are viruses, or yourself
filterUpdates :: GameInfo -> [Update] -> [Update]
filterUpdates gi updates = filter valid updates
    where valid o = ((player_id o) /= (myID gi)) && (not $ virus o)

-- TODO handle removes and eats
go :: GameInfo -> IO ()
go gi@(GameInfo { targetPoint = Nothing }) = do
        print "picking a target"
        -- it is possible to get stuck here, the game won't update you
        -- unless you move, so just send a move in some direction
        message <- readChan (incoming gi)
        case message of
            WorldUpdate _ updates _ -> let ups = (seen gi) ++ updates in
                                       go gi { targetPoint = maketp gi ups
                                             , myLocation  = newLoc updates
                                             , seen        = filterUpdates gi ups }

            _                       -> go gi

    where maketp gi ups = getClosest (myLocation gi) $ map loc $ filterUpdates gi ups
          newLoc updates = case getMyLocation (myID gi) updates of
                               Just p  -> p
                               Nothing -> (myLocation gi)


go gi@(GameInfo {myLocation = me, targetPoint = Just tp}) =
        if withinTolerance me tp
            then go gi { targetPoint = Nothing
                       , seen = filterUpdates gi $ filter (\s -> (loc s) /= tp) (seen gi) }
            else do
                writeChan (outgoing gi) (SetDirection tp)

                -- wait for an update before proceeding.
                -- change our location if we get a world update
                message <- readChan (incoming gi)
                case message of
                    WorldUpdate _ updates _ -> go gi { myLocation = newLoc updates
                                                     , seen       =
                                                         filterUpdates gi $ (seen gi) ++ updates }
                    _                       -> go gi

    where withinTolerance a b = distance a b <= 10.0
          newLoc updates = case getMyLocation (myID gi) updates of
                               Just p  -> p
                               Nothing -> (myLocation gi)
