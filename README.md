# Haskell Client for agar.io
This may eventually become a means to program bots for
[agar.io](http://agar.io). At the moment, it only (sort of) works with the open
source agar.io server clone [Ogar](https://github.com/OgarProject/Ogar), but I
have connected to a real agar.io server at least once and gotten something sort
of reasonable back.

The official agar.io client can connect to an Ogar server and play the game like
it normally would, so it's probably not that far from reality.

## High Level Overview
agar.io uses websockets to communicate with the browser, but they use an
undocumented binary protocol. Luckily, people have
[reverse engineered](http://agar.gcommer.com/index.php?title=Protocol) a good
amount of the protocol. The Ogar server also provides a reference for the
protocol.

### AgarFeed module
The `AgarFeed` module provides input and output to the agar server through some
ADTs defined in the module.

The binary serialization and deserialization happens in this module.

The feed is accessible to clients of different types:

* Raw clients - receive ByteStrings containing the raw binary data (useful for
  debugging)
* Message Clients - receive messages decoded into the native haskell types

The feed can send messages to an arbitrary number of clients. Messages are sent
to clients using `Control.Concurrent.Chan` synchronized queues. It is assumed
that each client will be running in it's own thread, consuming the messages as
they come in on their respective queue.

Output to the agar.io server is also provided via a queue. The feed reads from
its `command queue` and sends any messages (expressed in haskell types) received
via the command queue to the server.

This means that one feed connection can only run one bot which actually
interacts with the game, but it can host multiple loggers or analysis clients
(each running in their own threads)

### Client modules
Example of a simple client:
```
client :: Chan AF.IncomingMessage -> Chan AF.OutgoingMessage -> IO ()
client incoming outgoing = do
        -- read messages forever until we get the game dimensions
        -- once we get the game dimensions, start going in circles
        forever $ do
            message <- readChan incoming
            case message of
                AF.GameAreaSize gmin gmax -> startBot outgoing gmin gmax
                _                         -> return ()
```

This client is a message client. It waits to receive a certain type of message,
then calls some other function to actually kick off the both.

The bot has some logic (there are probably examples of this in `src/Clients`),
but the key part is:

```
writeChan outgoing (SetDirection x y) -- start going this direction
```

This line (or something like it) sends the SetDirection message to the agar
server, via the agar feeds command channel.
