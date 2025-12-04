# Advent Of Code 2025

This folder contains implementation of some puzzles given in [AoC 2025](https://adventofcode.com/2025).
The repository is structured as follows

```
|- bin/
|  |- day01
|  |- day02
|  |- ....
|  \- day25
|- cmd/
|  |- day01/
|  |  \- main.go
|  |- ...
|  \- day25/
|     \- main.go
|- Makefile
|- pkg/
|  |- utils/
|  \- pkg/
\- inputs
   |- day01/
   |  |- test.txt
   |  \- test2.txt
   |- ....
   \- day25/
      \- test.txt
```

The directories in question then contain following:

- `bin/` contain produced binaries - all of them are compiled by invocation of `make` command.
- `cmd/` contains tree structure for all executable files - one per puzzle - named as `day[0-9]{2}`
  which contains single text file `main.go`
- `pkg/` utilitary go modules for shared functions - e.g. graph functions, command line arguments
  and input file processing
- `inputs/` is a structure where to put all the inputs related to the puzzle testing.
- `Makefile` describes how to build and execute days with their respective targets.

## Prerequisities

In order to run this repository you don't need much, the source code is written in pure go without any
external dependencies, only the [go standard library](https://pkg.go.dev/std) is used. 
For convenience and ease of usage, it is also recommended to use [GNU make](https://www.gnu.org/software/make/).

## Running particluar test or input

To run solution for specific day against a particural input there are two possibilites

- Put the input file into the `inputs/day??` directory for the respective day and then invoke
  `make inputs/day$DAY_NO/$INPUT_FILE`, which will build (if doesn't exist) the binary for 
  selected day and then execute it with the `$INPUT_FILE` provided as input.
- Build the executable manually and then run it as `bin/dayXX -inFile $INPUT_FILE`

To run for your personal input, just run `make dayXX` which will execute the binary witout any
input, this will cause the solution to pull the input from AOC website, note that you'll need your
access cookie (from web debugger in browser) set to `AOC_AUTH` environment variable. To limit number of
apicalls, the solution will store this data into the file `inputs/dayXX/input.txt` so it is not redownloaded
for every test run.

On some particular days the solution is not as straight forward - not simply computable - as for others
and some outside inputs might be needed. Every program can have its own set of commandline options,
which can be passed to modify its behaviour. To invoke such commands using make targets, just
pass the `ARGS` variable to the make invocaion line and these arguments will be added.

## License

This code is my personal creation and as such is available under MIT license (see the LICENSE.md). All the credit
for the definition of the problems and great overarching story must go to the [Eric Wastl](https://adventofcode.com/2024/about)
with gratitude and admiration.
