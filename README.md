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

## Strategy

Heuristics and limited brute-force search for moves that fill a
complete row.

## Ressource use

Our player usually needs about 500MB of memory.

It uses up to 6 threads regardless of the `-c` command line flag.
