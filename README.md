vectorracing
============

Implements this game:
http://en.wikipedia.org/wiki/Racetrack_(game)

Demo:
https://asciinema.org/a/7p2vhtzic4sdpa3pdkysbxq69

TODO:

* UI to Select AI difficulty (and number of players?)
* Clean up AI module.  Each tree node doesn't need an independent notion of history (CarState already has this)
* Parametrize Point to take any numeric type, instead of only Floats...
* The move-selection could provide some sort of immediate feedback to the user, before they commit to the move.  Maybe show the delta on the screen, or show the projected result of choosing a given move.  Again, use *colors*
