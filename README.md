# EDDA

Build Dependencies:

- ZeroMQ v4.x

Create database indexes:

    db.systems.createIndex({edsmId: 1})
    db.systems.createIndex({systemName: 1})
    db.stations.createIndex({systemName: 1, stationName: 1}, {unique: true})
    db.stations.createIndex({systemName: 1})
    db.stations.createIndex({stationName: 1})
    db.stations.createIndex({eddbId: 1})

Import initial static data from EDDN:

    curl -O https://raw.githubusercontent.com/jamesremuscat/EDDN/master/doc/commodity.csv
    mongoimport -d edda --headerline --type=csv -c commodity --upsert commodity.csv

    curl -O https://raw.githubusercontent.com/jamesremuscat/EDDN/master/doc/outfitting.csv
    mongoimport -d edda --headerline --type=csv -c outfitting --upsert outfitting.csv

    curl -O https://raw.githubusercontent.com/jamesremuscat/EDDN/master/doc/shipyard.csv
    mongoimport -d edda --headerline --type=csv -c shipyard --upsert shipyard.csv
