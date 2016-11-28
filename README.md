O-MI Watchdog Tool
==================

*Related to standards Open Messaging Interface (O-MI) and Open Data Format (O-DF)*

This tool is used to detect if O-DF InfoItems are not updating on O-MI Nodes.
It accomplishes that by tracking the update intervals of all items when they still update 
and then alerts if the current interval will be greater than `avarageInterval + 3 * stdDeviationOfIntervals`.

The tool currently fetches all InfoItems with "read <Objects>" O-MI request 
which is assumed to return structure containing all InfoItems and (at least) one value for each.


Compiling
---------

Uses the `cabal-install` build system.

Some dependencies might be broken, they can be unpackaged by
`cabal unpack <package>` and then fixed and installed `cabal install <dir>`.

_TODO: change to `stackage` for easier compiling._

Run:

`$ cabal install`

Usage
-----

**Testing:**

`$ ./OmiWatchdog <url of an O-MI Node>`

It fetches all items once, does interval calculations, saves the results to the
state database, checks for alerts, prints the alerts and exits. So run it a
couple times.

**Production:** Should be run with cron or similar.

**To run a command for every InfoItem that changes state:**

```
$ crontab -e

*/5 * * * * cd OmiWatchdog && ./OmiWatchdog <url of an O-MI Node> | xargs -n 2 <program to run>
10 1 * * * rm -r OmiWatchdog/state/DelayStore/Archive/
```
xargs splits the lines (2 arguments per each: Event and O-DF path) and runs the program for every event


**To run the given IFTTT connector script:**
```
$ crontab -e

*/5 * * * * cd OmiWatchdog && ./OmiWatchdog <url of an O-MI Node> | ./sendWatchdogNotifications.sh -
10 1 * * * rm -r OmiWatchdog/state/DelayStore/Archive/
```


The program requires write permissions in its working directory
to store its state to directory `./state/`.

**NOTE**: Storage does not contain the url of the origin node so all
calls from the same working directory saves the data to same
storage. This means it will aggregate all data from all calls in
the same directory.


IFTTT Script
-----------

There is an experimental script to send the results to IFTTT: `./sendWatchdogNotifications.sh`

It's setup to use `Maker` channel in IFTTT:

* three events: `Online`, `Missing`, `Lost`,
* value1 is the O-DF path of the InfoItem
* value2 is the url for data discovery
* value3 is textual description of what happened

Setup required:

1. Create the three events in IFTTT and connect them to something (we tried Slack)
2. Edit the script with your Maker channel API key. You can get it from https://internal-api.ifttt.com/maker
3. Setup cron to call the script as specified in [Usage section](#Usage)

Read the script for furher info.


The state database
----------------------

Important data stored in directory `./state/DelayStore/`

Unneeded old data (backups) are moved to directory `./state/DelayStore/Archive/`
WARNING: Remember to remove archived files with cron or similar, otherwise you might run out of disk space!

Future ideas
------------

* Use a listening O-MI service with an event subscription on the watching target
  O-MI node.
* Aggregate the results to parent Object if all child InfoItems are giving the
  same alert
* Use stackage for compiling so other than Haskell coders can compile it

