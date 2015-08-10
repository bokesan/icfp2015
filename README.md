# ICFP programming contest 2015 entry

Folders:

    src/
        java player, requires Java 8 and ant for building.
    web/
        html+javascript implementation for experimentation
    haskell/
        Haskell implementation.

Our main implementation is written in Java.

The [web version](web/index.html) was created mainly to experiment with
the rules, but also includes a simple solver.

The Haskell version was written during the lightning phase, because we
thought
it might be easier to code advanced strategies in Haskell than in
Java. It was no longer maintained after we decided to stay with Java.

## Usage

We call out Java version with a bash script that supports the flags
from the task specification.

There are some additional flags:

TODO: describe them.


## Strategy

Heuristics and limited brute-force search for moves that fill a
complete row.

### Phrases of Power

We found only 8 phrases during the contest.

TODO: describe strategy.

## Ressource use

Our player usually needs about 500MB of memory.

It uses up to 6 threads regardless of the `-c` command line flag.
