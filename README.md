# AOC 2021

These are my solutions to [Advent of Code](https://adventofcode.com)
2021, written in Haskell.

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
