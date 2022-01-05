## `net-mqtt` message ordering reproduction

Run `docker-compose up -d` to start a Mosquitto instance, then `cabal run`. \
Change the `callback` type in the program to see the difference.

### Performance
Using GHC 8.10.7 on a AMD Ryzen 9 3900X 12-Core Processor

#### `-O2 -threaded`
SimpleCallback: 8 seconds
OrderedCallback: 10 seconds

#### `-O2 -threaded "-with-rtsopts=-N"`
SimpleCallback: 7 to 9 seconds
OrderedCallback: 9 to 11 seconds