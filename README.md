O-MI Watchdog Tool
==================

Compiling
---------

Uses the `cabal-install` build system.

Some dependencies might be broken, they can be unpackaged by
`cabal unpack <package>` and then fixed and installed.

TODO:
change to `stackage` for easier compiling.

Run:

`$ cabal install`

Usage
-----

Testing:

`$ ./watchdog <url of an O-MI Node>`

Production: Should be run with cron or similar:

```
$ crontab -e

*/5 * * * * ./watchdog <url of an O-MI Node> | xargs -n 2 <program to run>
```
xargs splits the lines (2 arguments per each: Event and O-DF path) and runs the program for every event

The program requires write permissions in its working directory
to store its state to directory `./state/`.

NOTE: Storage does not contain the url of the origin node so all
calls from the same working directory saves the data to same
storage. This means it will aggregate all data from all calls in
the same directory.

Runtime state database
----------------------

Important data stored in directory `./state/DelayStore/`

Unneeded old data (backups) are moved to directory `./state/DelayStore/Archive/`

