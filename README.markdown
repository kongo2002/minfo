# minfo - tool to analyze mongodb log files

[![build](https://api.travis-ci.org/kongo2002/minfo.png)][travis]


## Building

You can build **minfo** using *cabal*:

    $ cabal install --dependencies-only
    $ cabal configure
    $ cabal build


## Usage

Usually you invoke **minfo** just by passing it one of your [MongoDB][mongodb]
log files:

    $ minfo /var/log/mongodb.log

You may pass [MongoDB][mongodb] logs via *stdin* as well:

    $ zcat /var/log/mongodb.log.gz | minfo


### Output

At the moment **minfo** produces output like the following:

    NS:                  COUNT:     MIN:       MAX:       AVG:       SUM:
    test.Users           3          105        178        137.67     413
        { "id": 1 }
    test.Users           1          113        113        113.00     113
        { "fn": 1, "ln" : 1 }


### Sorting

You can sort the output via the `--sort` or `-s` parameter like the following:

    $ minfo -s max /var/log/mongodb.log

The order may be one of `min`, `max`, `avg` or `sum` and defaults to `sum`.


[travis]: https://travis-ci.org/kongo2002/minfo/
[mongodb]: http://www.mongodb.org/
