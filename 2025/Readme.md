# Advent Of Code 2025

This folder contains implementation of some puzzles given in [AoC 2025](https://adventofcode.com/2025).
The repository is structured as follows

```
|- bin/
|  |- day01
|  |- day02
|  |- ....
|  \- day12
|- cmd/
|  |- day01/
|  |  \- main.go
|  |- ...
|  \- day12/
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
   \- day12/
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

## Notes on implementation

Vast majority of implementations are straight and quite simple algorithms to solve, maybe suboptimally, the problem and run quite fast, 
there is problem only for several implementations.

### Day 09

This is by far least efficient implementation here, which takes around 13 seconds. The reason is that in the end, the
algorithm tries all tuples of points to verify, if it creates full rectangle. To do that it verifies every single
`y` coordinate if it creates full line (e.g. it's coordinates are within one continous ragne on the line). 
The program could've been optimized on several levels:

* First implementation is to transform the polygon representation into rectangles instead of array of ranges for
  each `y` coordinate.
* Since for every single corner, we know in which direction there is (e.g. from 4 quarters around every single point)
  only one which is tiled, we can eliminate all points in 3 quarters (e.g. for points near center up to 3/4) so on average
  something between 1/2 and 2/3 of points can be eliminated from consideration.

But given the time required to implement such optimizations it seems to me as a good trade-off to have the 13 s long
execution.

### Day 10

Boy, oh boy, this one was a tough one to implement in pure Go. So the series of bad decisions start at the point
I realized, that the problem of Part II describes actually [System of linear equations](https://en.wikipedia.org/wiki/System_of_linear_equations)
and so the linear programming is the way to go.

And there the fun starts, because there are many not that many inputs, that would have one single solution, but majority of those
have solution within a linear subspace, which makes this as an optimization problem, but I'm getting ahead of myself.

So for a starters, the program transforms the input into the matrix of Rational numbers, once this is done
it will start [Gaussian elimination](https://en.wikipedia.org/wiki/Gaussian_elimination) to build matrix in
triangular shape. Fortunately all inputs are natural nubmers, so by process of GEM, it can only produce
Rational numbers, which makes the program much more simple. When the matrix is in triangular format
the more funny part starts:

* In the case there is only one solution (e.g. every column apart from the results one is main column) it just
  returns it.
* In the case there are some variables that can't be directly evaluated those must be set as parameters, describing
  linear subspace of the solution, and this makes is into an optimization problem. Fortunately the following statements
  are true:
  * Every variable for a valid solution must be a whole, non negative number.
  * The sum of the variable is minimal (optimization criterion)

Once the linear subspace is described, based on parameters necessary then the solution switches to the brute-force
algorithm of trial and error of the combinations. Since the number of solutions is infinite (each parameter lies on 
$N_0$ because any variable is non-negative whole number) we need some way to figure out the path through the infinite
linear subspace. I've decided to use 1-norm of the vector, going from vectors with 1-norm equal to 0 (e.g. only vector $(0,0,0)$ )
up to some arbitrary number where all solutions are defined and minimal (in case of my input it was 200). This is
where the algorithm becomse brute force, for vectors in $Q^3$ with $1$-norm of 200 there is `20301` distinct vectors
and to try all `1-norm` vectors from $(0,200)$ it needs `4 080 501` vectors to try. This makes this implementation to 
run for 4 seconds.

The solution could've been more optimized by eliminating all inefficient parameter increments - for many equations, the
parameter in use is multiplied by a rational number which makes result unfit, so those can be skipped altogether.
Another optimization could be to detect algorithm stalling (when the minimum is found), because there are two options (with growing
`1-norm`), the series of solutions will either be growing - e.g. any minimum can't be found anymore or it will go below 0 for any
variable, so it breaks fundamental constraint.

### Day 11

This one was a head ache as well unless you do some observation with the graph - original implementation was general 
algorithm finding all paths between nodes in general graph - works for part I - which is NP-complete problem - 
so the recursive implementation for Part II took about 12 hours without solving even the first sub-problem. 

But once you read correctly the hint in the puzzle - [the dag one](https://en.wikipedia.org/wiki/Directed_acyclic_graph) 
the problem of finding all paths transforms from `NP` to a quadratic one (to be precise $O(V + E)$ ).

### Day 12

Ok this one is basically not solved, at least not in the conventional sense of the word because naming this
as a `NP` problem would be a great simplification. So instead of trying to find exact answer to the plane
tiling problem with a strings of irregular shapes, the program will just print lower and upper bounds
where the solution lies:

* Lower bound is (guaranteed hits) is the number of planes where the solution will definetly fit, because
  it can fit all 3x3 squares without any rotations or reflections.
* Upper bound is the number of all planes that can fit the area - so maximally compressed packages.

The rest is up to the user to derive some sense from those numbers.


## License

This code is my personal creation and as such is available under MIT license (see the LICENSE.md). All the credit
for the definition of the problems and great overarching story must go to the [Eric Wastl](https://adventofcode.com/2024/about)
with gratitude and admiration.
