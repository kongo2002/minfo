# minfo - tool to analyze mongodb log files

[![build](https://api.travis-ci.org/kongo2002/minfo.png)][travis]


## Building

You can build **minfo** using *cabal*:

    $ cabal install --dependencies-only
    $ cabal configure
    $ cabal build


## Usage

As of now **minfo** supports two operations:

- *queries*: this command filters for [MongoDB][mongodb] query operations
  including updates, inserts and commands. Those operations are aggregated and
  grouped by their namespace and runtime of the command.

- *connections*: using this operation you can observe the number of connection
  establishments and disconnects grouped by the client IPs.


### Queries

Usually you invoke **minfo** just by passing it one of your [MongoDB][mongodb]
log files:

    $ minfo queries /var/log/mongodb.log

You may pass [MongoDB][mongodb] logs via *stdin* as well:

    $ zcat /var/log/mongodb.log.gz | minfo queries


#### Output

At the moment the 'queries' operation of **minfo** produces output like the
following:

    NS:                  COUNT:     MIN:       MAX:       AVG:       SUM:
    test.Users           3          105        178        137.67     413
        { "id": 1 }
    test.Users           1          113        113        113.00     113
        { "fn": 1, "ln" : 1 }


#### Sorting

You can sort the output via the `--sort` or `-s` parameter like the following:

    $ minfo queries -s max /var/log/mongodb.log

The order may be one of `min`, `max`, `avg` or `sum` and defaults to `sum`.


### Connections

The 'connections' operation may be invoked just like the 'queries' command:

    $ minfo connections /var/log/mongodb.log


#### Output

You may expect output for the 'connections' operation like this:

    IP:                  CONN:        DISCONN:
    127.0.0.1            351          350


[travis]: https://travis-ci.org/kongo2002/minfo/
[mongodb]: http://www.mongodb.org/
