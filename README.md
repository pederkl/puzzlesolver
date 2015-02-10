# Puzzlesolver
Hack to find solutions to an 8x8 puzzle with 13 pieces of various shapes.

Developed with LispWorks.  Should be mostly portable Common Lisp, but zero
effort has been spent verifying this.

## How to use

There is no binary to build and run.  From a Lisp REPL, do

1. `(asdf:load-system :puzzlesolver)`
2. `(in-package :puzzlesolver)`
3. `(solve-puzzle)`

Solve-puzzle takes around 5 hours on my laptop, and writes the solutions found
to `*standard-output*`. That's a lot of text, there are over 129.000 solutions
found (some are probably duplicates or simple board rotations).

## Customization

If you have a puzzle with the same concept, but different board geometry and pieces,
this hack is probably adaptable.  Have a look at data.lisp.

## Ideas for improvement

* Collect solutions in a datastructure instead of printing them all out.
* Pretty graphical output of solutions.  The current block of characters is ugly.
* Pretty graphical gui for modifying piece and board shapes.

This was a one-off hack, and these ideas will probably never materialize.
