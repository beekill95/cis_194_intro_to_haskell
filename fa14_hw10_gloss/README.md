# Homework 10: Gloss

## Dependencies

This homework depends on `gloss` package,
so make sure that you have `gloss` installed:

> cabal install --lib gloss=1.13.*

Before that, however, `gloss` depends on several system libraries:
libGL-devel, libGLU-devel, and freeglut-devel.
I'm using Fedora, so these can be installed using:

> sudo dnf install mesa-libGL-devel mesa-libGLU-devel freeglut-devel

## Running

In order to run the program, use:

> cabal run

On my system, however, it throws error:

```
fa14-hw10-gloss: mmap 4096 bytes at (nil): Cannot allocate memory
fa14-hw10-gloss: Try specifying an address with +RTS -xm<addr> -RTS
```

~~I'm not sure what the error means or whether you'll encounter a similar one.
I don't have a fix for this, so for me, repeatedly execute `cabal run` works.~~
Upgrading GHC to version >= 9.4 according to
[this](https://discourse.haskell.org/t/facing-mmap-4096-bytes-at-nil-cannot-allocate-memory-youre-not-alone/6259)
solves the problem for me.