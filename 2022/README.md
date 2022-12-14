# Advent Of Code 2022

This repository contains solution of [AoC 2022](https://adventofcode.com/) in [Haskell](https://www.haskell.org/) language.

To run these programs you need:

- [GHC](https://downloads.haskell.org/ghc/latest/docs/users_guide/index.html)
- [cabal](https://www.haskell.org/cabal/)

In order to execute particular solution just run `cabal run` like in next example:

```bash
cabal run dayXX -- input.txt
```

Where:

- `dayXX` is the day you want executed.
- `input.txt` is the input file in format expected by the respective day assignment (e.g. the file you downloaed)

## Disclaimer

This project can be in no way considered as properly coded Haskell. There are a lot of bugs, the naming is
inconsistent and there are things that are surely against best practices. Also I still don't fully understand
monadic data types so expect that in a lot of places the `<-` and `>>=` operators will be badly used.

## License
This code is licensed under MIT license.
