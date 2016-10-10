O-MI Watchdog Tool
==================

Compiling
---------

Uses the `cabal-install` build system.

Some dependencies might be broken, they can be unpackaged by
`cabal unpack <package>` and then fixed and installed. TODO:
change to `stackage` for easier compiling.

run

`$ cabal install`

Usage
-----

`$ ./watchdog <url of an O-MI Node`

The program requires write permissions in its working directory
to store its state to directory `./state/`.

NOTE: Storage does not contain the url of the origin node so all
calls from the same working directory saves the data to same
storage. This means it will aggregate all data from all calls in
the same directory.
