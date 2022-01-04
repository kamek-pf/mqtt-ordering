## `net-mqtt` message ordering reproduction

Run `docker-compose up -d` to start a Mosquitto instance, then `cabal run`. \
Change the `callback` type in the program to see the difference.