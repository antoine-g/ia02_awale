# Awale

This is a **Prolog project** achieved for a "Logical programming and problems resolution" course (IA02).

It follows all classical gaming rules, see [Wikipedia article](https://en.wikipedia.org/wiki/Awale).
The game is played in CLI and handles human-vs-human, human-vs-AI or AI-vs-AI.
If the player is human, a help command gives a hint on a possible move.

AI moves are partially randomized to be less predictable.
The artificial intelligence only forsees one move in advance but it is enough for regular players.

## Play

To run the program you need to use a Prolog interpreter such as GNU Prolog ([http://www.gprolog.org/](http://www.gprolog.org/)).

* Move to the directory containing the file
```
change_directory("path/to/directory").
```
* Load the `awale.pl` file
```
consult("awale.pl").
```
* Use the `init.` command to launch the game

## Licence
Anaig Maréchal, Antoine Giraudmaillet

Under the terms of the GNU General Public License, version 3 (GPL-3.0).
Full terms can be found at [https://www.gnu.org/licenses/gpl.html](https://www.gnu.org/licenses/gpl.html).
