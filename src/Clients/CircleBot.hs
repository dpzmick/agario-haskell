-- this client controls the ball to move in a circle
-- it doesn't care at all where we start, it just pretends it is in the
-- center of the map, then moves in a circle from there

module Clients.CircleBot (client) where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan

-- to get the unsigned word32
import Data.Word

import qualified AgarFeed as AF

client :: Chan AF.IncomingMessage -> Chan AF.OutgoingMessage -> IO ()
client incoming outgoing = do
        -- read messages forever until we get the game dimensions
        -- once we get the game dimensions, start going in circles
        forever $ do
            message <- readChan incoming
            case message of
                AF.GameAreaSize gmin gmax -> startBot outgoing gmin gmax
                _                         -> return ()

----------------------------------------------------------------------------

-- we get the coordinates as a double, but we must send integers for the
-- direction.
-- this doesn't really show it's face this function, but it will in
-- circleCoords

startBot :: Chan AF.OutgoingMessage -> AF.Point Double -> AF.Point Double -> IO ()
startBot o (AF.Point (xmin, ymin)) (AF.Point (xmax, ymax)) = do
        -- find the center of the map (these are doubles)
        let (xc, yc) = ( (xmax + xmin) / 2 , (ymax + ymin) / 2)
        let radius   = 100.0

        -- never read any more messages, just start sending SetDirection
        -- messages to move in a circle
        let
          loop (x, y) degrees = do
            print (x, y)                      -- do some logging
            writeChan o (AF.SetDirection x y) -- start going this direction

            -- sleep for a bit to avoid destroying the server
            threadDelay 100000

            -- next iteration
            let new_angle = (degrees + 10) `mod` 360
            loop (circleCoords radius new_angle (xc - radius) yc) new_angle

        -- start the loop
        -- the center coords are doubles, so we will just floor them
        loop (floor (xc - radius), floor yc) 0


-- does all the math in doubles, then rounds down at the end
circleCoords :: Double -> Int -> Double -> Double -> (Word32, Word32)
circleCoords radius degrees x0 y0 = (floor x, floor y)
    where d = fromIntegral degrees
          x = x0 + radius * cos (d * (pi / 180))
          y = y0 + radius * sin (d * (pi / 180))
