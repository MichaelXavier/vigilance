# Vigilance
[![Build Status](https://travis-ci.org/MichaelXavier/vigilance.png?branch=master)](https://travis-ci.org/MichaelXavier/vigilance)

Vigilance is a [dead man's switch](https://en.wikipedia.org/wiki/Dead_man%27s_switch)
(or vigilance switch). You define named **watches** that you expect to happen
and how long to wait inbetween before it's time to worry. You then instrument
your periodical tasks, whatever they are, to report to vigilance via a simple
HTTP POST. You can then configure notifications that will fire when a watch
fails to check in.

# vigilance-server
vigilance-server is the server component of vigilance. It is responsible for
tracking what watches there are, their state, notifications, etc.

## Usage
Simply run `vigilance-server path/to/config.cfg`.

## Configuration
The configuration file is in
[configurator](http://hackage.haskell.org/package/configurator) format. Here's
an example config

### Full Config
```
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
```

Note that like the standard capabilities configurator has to expand env
variables and  load external config files apply:

```

vigilance {
  acid_path  = "$(HOME)/vigilance-data"

  watches {
    import "only_watches.conf"
  }
}
```
### Limited Config Reload Support
Sending a `HUP` signal to the process (`kill -HUP pid_of_vigilance`) will
reload the config. Reloading while running can currently update the following
settings:

1. Log verbosity.
2. List of watches
3. Log location

### Default Config
The following values are defaulted if not supplied by the config:

1. `vigilance.port` - 3000
2. `vigilance.acid_path` - state/AppState
3. `vigilance.from_email` - None. Will disable emails.
4. `vigilance.max_retries` - 3. Number of times a notification will retry.
5. `vigilance.log.verbose` - no
6. `vigilance.log.path` - log/vigilance.log
7. `vigilance.watches` - none

## API

# Vigilance Client
Vigilance Client is available under the `vigilance` binary. It allows you to
interact with a vigilance server over HTTP in a concise way. The idea behind
this is that it should make it very easy to insert check-ins in crontabs and
shell scripts.


# Configuration
Vigilance by default looks for a `.vigilance` file in your home directory,
which looks like:

```
vigilance
{
  host = "localhost"
  port = 3000
}
```

# Usage
Run `vigilance --help` for help:
```
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
```

All commands except `list` take a name argument for the watch like: `vigilance
pause foo`.

## Status
Currently unreleased and under active development, but I'm getting close. See
`TODO` for some notes on what I need to do.

## License
Vigilance is released under the MIT license. See the `LICENSE` file for more
info.
