vectorracing
============

Implements this game:
http://en.wikipedia.org/wiki/Racetrack\_(game)

TODO:

* UI to pick between multiple courses
* Concept of "finish line" to know when you got to the end
* Select AI difficulty (and number of players?)
* Rendering.  The current approach for rendering walls is: "if a cell is near a line-segment representing a wall, render it as a wall".  The thing about a console UI is that it has *very low resolution*, so we wind up marking non-walls as walls.  This is no good.  Figure out a solution.
* Render dust-trail using multiple *colors*
* The move-selection could provide some sort of immediate feedback to the user, before they commit to the move.  Maybe show the delta on the screen, or show the projected result of choosing a given move.  Again, use *colors*
