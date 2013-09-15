# Vigilance
[![Build Status](https://travis-ci.org/MichaelXavier/vigilance.png?branch=master)](https://travis-ci.org/MichaelXavier/vigilance)

Vigilance is a [dead man's switch](https://en.wikipedia.org/wiki/Dead_man%27s_switch)
(or vigilance switch). You define named **watches** that you expect to happen
and how long to wait inbetween before it's time to worry. You then instrument
your periodical tasks, whatever they are, to report to vigilance via a simple
HTTP POST. You can then configure notifications that will fire when a watch
fails to check in.

# Concepts
A *watch* is a named task that you expect to happen periodically. Watches can
be in several states:

* *Active* - The clock is ticking but this watch has not yet triggered.
* *Paused* - The clock is not ticking. **Watches start out in this state.**
  That means that you must unpause the watch to start monitoring this. I
  thought about automatically unpausing on checkin but that would mean to pause
  notification you'd have to eliminate all checkins.
* *Notifying* - The watch has failed to check in and will notify soon.
* *Triggered* - The watch has failed to check in and has notified. It will not
  notify until it is dealt with, either by pausing, checkin in on removal.

Watches are configured in the server's config file and managed via the rest API
or the vigilance client. The configuration file can be reloaded to account for
changes/additions/removals of watches.

Watches can be configured with multiple *notifications* to fire when the watch
fails to check in. Right now the supported notification options are:

* Email
* HTTP POST

# vigilance-server
vigilance-server is the server component of vigilance. It is responsible for
tracking what watches there are, their state, notifications, etc.

## Usage
Simply run `vigilance-server path/to/config.cfg`. If you don't specify a
config, it will look in `~/.vigilance/server.conf`

## Configuration
The configuration file is in
[configurator](http://hackage.haskell.org/package/configurator) format. Here's
an example config

### Example Config

    vigilance {
      port = 9999
      from_email = "vigilance@example.com"
      max_retries = 5
      log {
        verbose = on
        path = "log/vigilance.log"
      }
      watches {
        foo {
          interval = [2, "seconds"]
          notifications = [
            ["http", "http://localhost:4567/notify"],
            ["email", "ohno@example.com"]
          ]
        }

        bar {
          interval = [3, "minutes"]
        }
      }
    }

Note that like the standard capabilities configurator has to expand env
variables and  load external config files apply:

    vigilance {
      acid_path  = "$(HOME)/vigilance-data"

      watches {
        import "only_watches.conf"
      }
    }

### Limited Config Reload Support
Sending a `HUP` signal to the process (`kill -HUP pid_of_vigilance`) will
reload the config. Reloading while running can currently update the following
settings:

1. Log verbosity.
2. List of watches
3. Log location

### Config Fields

| Field                          | Default                     | Description                                                 | Reloadable |
| ------------------------------ | --------------------------- | ---------------------------------------------------------   | ---------- |
| port                           | 3000                        | Server port                                                 | No         |
| from_email                     | None                        | Email  to send from. If missing, no email notifications     | No         |
| max_retries                    | 3                           | Max retries for notifications                               | No         |
| log.acid_path                  | ~/.vigilance/state/AppState |                                                             | No         |
| log.verbose                    | no                          | Verbose logging                                             | Yes        |
| log.path                       | ~/.vigilance/vigilance.log  |                                                             | Yes        |
| watches._name_.interval        | None. Required for a watch  | Pair of number and seconds/minutes/hours/days/weeks/years   | Yes        |
| watches._name_.notifications   | Empty                       | List of pairs ["http", "url"] or ["email", "a@example.com"] | Yes        |

## REST API
Vigilance exposes a REST API for managing watches.

| Path                    | Method | Description                                                                                  |
| ----------------------- | ------ | -------------------------------------------------------------------------------------------- |
| /watches                | GET    | Get the list of watches in JSON.                                                             |
| /watches/_name_         | GET    | Get info for a watch by name                                                                 |
| /watches/_name_         | DELETE | Delete a watch. Make sure to remove it from the config or it will return on config (re)load. |
| /watches/_name_/pause   | POST   | Take a watch out of operation.                                                               |
| /watches/_name_/unpause | POST   | Put a watch back in operation.                                                               |
| /watches/_name_/checkin | POST   | Check in a watch.                                                                            |
| /watches/_name_/test    | POST   | Synchronously fire a watch's notifications. Returns a list of failures in JSON.              |

# Vigilance Client
Vigilance Client is available under the `vigilance` binary. It allows you to
interact with a vigilance server over HTTP in a concise way. The idea behind
this is that it should make it very easy to insert check-ins in crontabs and
shell scripts.


# Configuration
Vigilance by default looks for a `.vigilance` file in your home directory,
which looks like:

    vigilance
    {
      host = "localhost"
      port = 3000
    }

# Usage
Run `vigilance --help` for help:

    vigilance - tool for managing vigilance watches locally or remotely.

    Usage: vigilance COMMAND [-c|--config FILE]

    Available options:
      -h,--help                Show this help text
      -c,--config FILE         Config file. Defaults to ~/.vigilance

    Available commands:
      list                     List watches
      pause                    Pause watch
      unpause                  Unpause watch
      checkin                  Check in watch
      info                     Get info about a watch
      test                     Test the notifications for a watch

All commands except `list` take a name argument for the watch like: `vigilance
pause foo`.

## Status
Currently unreleased and under active development, but I'm getting close. See
`TODO.md` for some notes on what I need to do.

## License
Vigilance is released under the MIT license. See the `LICENSE` file for more
info.
