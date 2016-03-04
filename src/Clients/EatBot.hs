-- just tries to eat stuff (even if it's bigger than it)
-- doesn't try to not die

-- TODO figure out what removals do
-- TODO when a cell splits, each new cell has a unique player ID, which we
-- get in a OwnsBlob update
-- TODO someone we sometimes try to move off the map, not sure how
-- TODO exit when the feed shuts down

-- once this works, I'll probably strip all the code that maintains the
-- state of the game out and provide a nice interface into that state.
-- There probably won't be a performance problem, so this is almost
-- certainly fine to do.

module Clients.EatBot (client) where

import Control.Monad (forever)
import Control.Concurrent.Chan

import Data.Word
import Data.Maybe (isNothing)
import Data.List (sortBy, find, union)
import Safe (headMay)

-- import Debug.Trace

import AgarFeed

data GameInfo = GameInfo { myLocation  :: Point Word32
                         , myID        :: BlobID -- should be an array
                         , targetPoint :: Maybe (Point Word32)
                         , incoming    :: IQueue
                         , outgoing    :: OQueue

                         -- the game doesn't send you updates for things
                         -- you've already seen, unless they change.
                         -- cache the updates
                         , seen        :: [Update]
                         }


-- looks for an update giving your current location in the list of updates
-- given
getMyLocation :: BlobID -> [Update] -> Maybe (Point Word32)
getMyLocation bid updates = headMay $ map loc $ filter f updates
    where f o = (player_id o) == bid

-- looks for a new location for our bot in a set of updates, if it doesn't
-- find one, just return the location we already know about
newLoc :: GameInfo -> [Update] -> Point Word32
newLoc gi updates = case getMyLocation (myID gi) updates of
                        Just p   -> p
                        Nothing -> myLocation gi

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

-- removes updates that are viruses, or yourself
filterUpdates :: BlobID -> [Update] -> [Update]
filterUpdates bid updates = filter valid updates
    where valid u = ((player_id u) /= bid) && (not $ virus u)

-- if something gets eaten, remove it from our storage
removeEats :: [Eat] -> GameInfo -> GameInfo
removeEats eats gi = gi { seen = newSeen }
    where eater update u     = (victim_id u) == (player_id update)
          wasNotEaten update = isNothing $ find (eater update) eats
          newSeen            = filter wasNotEaten (seen gi)

-- gets a message, updates game info, then calls the function next with the
-- new game info as the only argument
handleUpdatesThen :: GameInfo -> (GameInfo -> IO ()) -> IO ()
handleUpdatesThen gi next = do
        -- print $ map player_id $ seen gi
        message <- readChan (incoming gi)
        handleSpecificUpdate gi message next

handleSpecificUpdate :: GameInfo -> IncomingMessage -> (GameInfo -> IO ()) -> IO ()
handleSpecificUpdate gi (WorldUpdate eats updates _) next = next noEats
    where newGi = gi { myLocation = newLoc gi updates }
          newSeen = filterUpdates (myID newGi) ((seen newGi) `union` updates)
          noEats = removeEats eats $ newGi { seen = newSeen }

-- TODO more logic here
handleSpecificUpdate gi _ next = next gi

----------------------------------------------------------------------------

-- TODO clean this up
client :: IQueue -> OQueue -> IO ()
client i o = forever $ do
        -- need to get our own id, then start playing
        print "waiting for id"
        message <- readChan i

        -- we should cache updates we see here
        case message of
            OwnsBlob blob_id -> inner blob_id
            _                -> return ()

    where
        tryToStart blob_id updates = do
            print $ getMyLocation blob_id updates

            -- we should cache updates here
            case getMyLocation blob_id updates of
                Just p  -> go GameInfo { myLocation  = p
                                       , myID        = blob_id
                                       , targetPoint = Nothing
                                       , incoming    = i
                                       , outgoing    = o
                                       , seen        = filterUpdates blob_id updates }
                Nothing -> return ()

        inner blob_id = forever $ do
          -- start moving a bit, to trigger a world update in case we missed
          -- it waiting for our id
          writeChan o (SetDirection (Point (0,0)))

          -- get our location
          message <- readChan i
          case message of
              WorldUpdate _ updates _ -> tryToStart blob_id updates
              _ -> return ()

----------------------------------------------------------------------------

go :: GameInfo -> IO ()
go gi@(GameInfo { targetPoint = Nothing }) = do
        print "picking a target"
        handleUpdatesThen gi (\ngi -> go ngi { targetPoint = maketp ngi })

    where maketp ngi = getClosest (myLocation ngi) $ map loc (seen ngi)

go gi@(GameInfo { outgoing = o, myLocation = me, targetPoint = Just tp }) = do
        print $ "I am at:             " ++ (show me)
        print $ "going to the target: " ++ (show tp)

        if withinTolerance me tp
            then handleUpdatesThen gi resetAndGo
            else do
                writeChan o (SetDirection tp)
                handleUpdatesThen gi go

    where withinTolerance a b = distance a b <= 10.0
          notPoint p update   = (loc update) /= p
          resetAndGo ngi      = go ngi { targetPoint = Nothing
                                       , seen = filter (notPoint tp) (seen ngi)}
