# EDDA

Elite Dangerous Data Aggregator is a [EDDN](https://github.com/jamesremuscat/EDDN) data collector that saves commodity/outfitting/shipyard data into MongoDB database and REST API service to query that data

## Build:

Build Dependencies:

- [ZeroMQ](http://zeromq.org/) v4.x

Build using [stack](http://haskellstack.org/):

    stack build

## Configure

Create new database called edda in MongoDB with indexes:

    use edda
    db.systems.createIndex({edsmId: 1})
    db.systems.createIndex({systemName: 1})
    db.stations.createIndex({systemName: 1, stationName: 1}, {unique: true})
    db.stations.createIndex({systemName: 1})
    db.stations.createIndex({stationName: 1})
    db.stations.createIndex({eddbId: 1})

Configure:

    vi edda.conf

Import initial systems/stations backup from [EDDB](https://eddb.io):

    stack exec edda -- import -s eddb -t systems
    stack exec edda -- import -s eddb -t stations

## Usage

Start edda data collector:
    
    stack exec edda -- start

Stop edda data collector:
    
    stack exec edda -- stop

Start edda REST API service:
    
    stack exec edda -- startRest

Stop edda REST API service:
    
    stack exec edda -- stopRest

See also:

    stack exec edda -- --help
