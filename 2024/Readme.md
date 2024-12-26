# Advent Of Code 2024

This year written in Golang!


This folder contains implementation of all puzzles given in [AoC 2024](https://adventofcode.com/2024).
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
- `inputs/` is a structure which contains all the inputs related to the puzzle - in this setup, it will
  only contain files named as `test?.txt` which are files in specified format extracted directly from
  the puzzles description, some might be related only to the partI, others to part II.
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

On some particular days the solution is not as straight forward - not simply computable - as for others
and some outside inputs might be needed. Every program can have its own set of commandline options,
which can be passed to modify its behaviour. To invoke such commands using make targets, just
pass the `ARGS` variable to the make invocaion line and these arguments will be added.

## Partially solved days

As entioned above some days are not solved E2E by the programm, but additional steps are required, 
specifically these are [Day 14](https://adventofcode.com/2024/day/14) and [Day 24](https://adventofcode.com/2024/day/24)
where the fully automated solution would mean (at least for me) much larger effort than to do a little
math or observation.

### Day 14

In case of day 14 the reason is simple, you need to watch for visual clues in the behaviour of the movement.
To facilitate this, the program will generate several (by default 1000) text files with step by step results
of the simulation. After inspection of these files, you'll see that with some periodicity two situations
happens and it can be reasonably assumed, that the searched `easter egg` will be at a time, hen both of
those situations happen simultaneously. This fact will lead you to the [Chinese remainder theorem](https://en.wikipedia.org/wiki/Chinese_remainder_theorem)
and with this knowlege, you'll have your output - which can be then easily verified by using
the `-res $RESULT` option, which will show you, if you've been sucessfull.

### Day 24

This one might be solvable by computable force, but I found it a little bit counterproductive and didn't want
to be bothered by that a day before christmas, so my solution went a little bit different way. 
Instead of trying to figure out where the swaps might have happended, the program will just render the
connections within the circuit. Since it is Adder, this circiuit must be quite "regular". So the program 
will generate a [Dot file](https://graphviz.org/doc/info/lang.html) which can then be transformed into
the picture by exectuting `dot -Tpng input.dot -o output.png`. Once I had this drawing, it took me around 15 minutes
to find the problematic connections. The program can perform the swaps for you, so once you'll identify some
problems in the connections, you can reexecute the day24 binary with option `-swaps I-J,K-L` which means
swap outputs I and J and K and L.

I reccomend to redreaw the dot file once you identify the first affecting pair, because it will create more
regular so the dot won't have such problems with drawing creation (because some complexity will be reduced - less crossings needed).

## Disclaimers

This code is in many cases suboptimal and in general badly programmed. The purpose is single, to solve the specific puzzle,
there were several attempts at refactoring during the course of Advent, some more and some less successful. 
In general vast majority of the codes will execute fairly quickly and will solve both of the tasks per day. 
In several instances the code contains two implementations solving the same problem one simple and naive which was enough
to solve the Part I and the other more complex and faster to solve also Part II. Both solutions were kept in place by me
to show the difference in approaches, notably it can be visible in `day20` where the basic solution iterating
over all possible cheats for Part I takes around 50 seconds on real data, while the more reasonable implementation
will do the same under 1 s for Part I and around 1 s for Part II.

## License

This code is my personal creation and as such is available under MIT license (see the LICENSE.md). All the credit
for the definition of the problems and great overarching story must go to the [Eric Wastl](https://adventofcode.com/2024/about)
with gratitude and admiration.

