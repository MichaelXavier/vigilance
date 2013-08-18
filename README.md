# Vigilance

Vigilance is a [dead man's switch](https://en.wikipedia.org/wiki/Dead_man%27s_switch)
(or vigilance switch). You define named **watches** that you expect to happen
and how long to wait inbetween before it's time to worry. You then instrument
your periodical tasks, whatever they are, to report to vigilance via a simple
HTTP POST. You can then configure notifications that will fire when a watch
fails to check in.

## Configuration

## Usage

## API

## Status
Currently unreleased and under active development, but I'm getting close. See
`TODO` for some notes on what I need to do.

## License
Vigilance is released under the MIT license. See the `LICENSE` file for more
info.
