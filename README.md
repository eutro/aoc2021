# AOC 2021

[![Run All](https://github.com/eutro/aoc2021/actions/workflows/run.yml/badge.svg)](https://github.com/eutro/aoc2021/actions/workflows/run.yml)

These are my solutions to [Advent of Code](https://adventofcode.com)
2021, written in Haskell. Each day can be found in the [`src`](src/)
folder.

## Building

Just run `ghc` manually on each day in the `src` folder.
Solutions take input from standard input.

There's some scripts to help:

- `./fetch.sh <day>`: fetch input for the given day, putting it in the
  `inputs` directory.
  - Requires `session.key` in the containing directory, which must
    contain your `session` cookie from the Advent of Code website.
- `./cmp.sh <day>`: compile the solution for the given day,
  outputting to the `out` directory.
- `./run.sh <day> [--]`: run the solution for the given day. Always
  recompile. If `--` is not provided, fetch input (with `fetch.sh`) if
  not yet downloaded. With `--`, just read input from stdin.

## What on earth does `(($) >>= (.) . ($ (>>)) . (.) . flip (.))` mean?

Most of the solutions have been rewritten in [point-free
style](https://en.wikipedia.org/wiki/Tacit_programming) for fun.
These solutions are not meant to be read, and most of them are, in
fact, quite incomprehensible. They are almost always entirely
equivalent to the normal, "pointful" solutions, which are preserved
below the point-free ones, usually with the function name `main1`.
