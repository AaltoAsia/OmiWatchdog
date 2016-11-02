O-MI Watchdog Tool
==================

Compiling
---------

Uses the `cabal-install` build system.

Some dependencies might be broken, they can be unpackaged by
`cabal unpack <package>` and then fixed and installed.

_TODO: change to `stackage` for easier compiling._

Run:

`$ cabal install`

Usage
-----

Testing:

`$ ./OmiWatchdog <url of an O-MI Node>`

Production: Should be run with cron or similar.

**To run a command for every InfoItem that changes state:**

```
$ crontab -e

*/5 * * * * cd OmiWatchdog && ./OmiWatchdog <url of an O-MI Node> | xargs -n 2 <program to run>
```
xargs splits the lines (2 arguments per each: Event and O-DF path) and runs the program for every event

**To run the given IFTTT connector script:**
```
$ crontab -e

*/5 * * * * cd OmiWatchdog && ./OmiWatchdog <url of an O-MI Node> | ./sendWatchdogNotifications.sh -
```


The program requires write permissions in its working directory
to store its state to directory `./state/`.

**NOTE**: Storage does not contain the url of the origin node so all
calls from the same working directory saves the data to same
storage. This means it will aggregate all data from all calls in
the same directory.

The state database
----------------------

Important data stored in directory `./state/DelayStore/`

Unneeded old data (backups) are moved to directory `./state/DelayStore/Archive/`

