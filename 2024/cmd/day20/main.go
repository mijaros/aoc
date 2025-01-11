package main

import (
	"container/heap"
	"container/list"
	"flag"
	"fmt"
	"maps"
	"math"
	"slices"
	"time"

	"github.com/mijaros/adventofcode/v2024/pkg/graphs"
	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

var (
	debugCounter map[int]int = map[int]int{}
	debugBase    int
	debug        bool
	maxNo        int
)

func DebugCounter(result, base, current int) int {
	if base-current >= maxNo {
		result++
		debugCounter[base-current]++
	}
	return result
}

func RealCounter(result, base, current int) int {
	if base-current >= maxNo {
		result++
	}
	return result
}

type Counter func(int, int, int) int

func PerformCheat(maze [][]byte, walls [][2]int, from, to [2]int) int {
	cpy := util.CopyMat(maze)
	for _, p := range walls {
		cpy[p[0]][p[1]] = '.'
	}
	r, _ := graphs.Dijkstra(cpy, from, to)
	if r < debugBase {
		cpy[walls[0][0]][walls[0][1]] = '1'
		cpy[walls[1][0]][walls[1][1]] = '2'
		for k := range maze {
			fmt.Printf("%s\t%s\n", string(cpy[k]), string(maze[k]))
		}
	}
	return r
}

func FindAllCheats(b [][]byte) [][2]int {
	res := make([][2]int, 0)
	for i := range b {
		for j := range b[i] {
			if isCheatPoint([2]int{i, j}, b) {
				res = append(res, [2]int{i, j})
			}
		}
	}
	return res
}

func RunAllCheats(maze [][]byte, start, end [2]int, base int, mapper Counter) int {
	res := 0
	allCheats := FindAllCheats(maze)
	for _, cheat := range allCheats {
		maze[cheat[0]][cheat[1]] = '.'
		mres, _ := graphs.Dijkstra(maze, start, end)
		res = mapper(res, base, mres)
		maze[cheat[0]][cheat[1]] = '#'
	}
	return res
}

type Tuple[F, S any] struct {
	first  F
	second S
}

func (t Tuple[F, S]) First() F {
	return t.first
}

func (t Tuple[F, S]) Second() S {
	return t.second
}

func NewTuple[F, S any](f F, s S) Tuple[F, S] {
	return Tuple[F, S]{first: f, second: s}
}

func TeleportMaxDistance(start [2]int, maze [][]byte, maxDist int) []Tuple[[2]int, int] {
	x, y := start[0], start[1]
	mx, my := len(maze), len(maze[0])
	res := make(map[Tuple[[2]int, int]]bool)
	for i := 0; i <= maxDist; i++ {
		for j := 0; j <= maxDist; j++ {
			if i+j > maxDist || i+j == 0 {
				continue
			}
			if x+i < mx {
				if y+j < my && maze[x+i][y+j] != '#' {
					res[NewTuple([2]int{x + i, y + j}, i+j)] = true
				}
				if y-j >= 0 && maze[x+i][y-j] != '#' {
					res[NewTuple([2]int{x + i, y - j}, i+j)] = true
				}

			}
			if x-i >= 0 {
				if y+j < my && maze[x-i][y+j] != '#' {
					res[NewTuple([2]int{x - i, y + j}, i+j)] = true
				}
				if y-j >= 0 && maze[x-i][y-j] != '#' {
					res[NewTuple([2]int{x - i, y - j}, i+j)] = true
				}
			}
		}
	}
	return slices.Collect(maps.Keys(res))
}

func inInterval(t, start, end int) bool {
	return t > start && t < end
}

func vertCheck(point [2]int, b [][]byte) bool {
	return b[point[0]][point[1]] == '#' && b[point[0]+1][point[1]] != '#' && b[point[0]-1][point[1]] != '#'
}

func horCheck(point [2]int, b [][]byte) bool {
	return b[point[0]][point[1]] == '#' && b[point[0]][point[1]+1] != '#' && b[point[0]][point[1]-1] != '#'
}

func isCheatPoint(p [2]int, b [][]byte) bool {
	if p[0] == 0 || p[0]+1 == len(b) || p[1] == 0 || p[1]+1 == len(b[0]) {
		return false
	}
	return vertCheck(p, b) || horCheck(p, b)
}

func DijkstraDistance(b [][]byte, start, end [2]int) (distances [][]int, parents [][][2]int) {
	h, w := len(b), len(b[0])
	distances = graphs.GenDistance(len(b), len(b[0]), math.MaxInt)
	parents = graphs.GenDistance(len(b), len(b[0]), [2]int{-1, -1})
	mheap := &graphs.DistHeap{{Vert: graphs.FromPoint(start), Dist: 0}}
	distances[start[0]][start[1]] = 0
	parents[start[0]][start[1]] = [2]int{start[0], start[1] - 1}
	heap.Init(mheap)
	closed := make(map[graphs.Vertex]bool)

	for mheap.Len() != 0 {
		p := heap.Pop(mheap).(graphs.Dist)
		if closed[p.Vert] {
			continue
		}
		closed[p.Vert] = true
		neighs := graphs.GenPoints(p.Vert.P, h, w)
		for _, n := range neighs {
			if b[n[0]][n[1]] == '#' || closed[graphs.FromPoint(n)] {
				continue
			}
			w := p.Dist + 1
			if w < distances[n[0]][n[1]] {
				distances[n[0]][n[1]] = w
				parents[n[0]][n[1]] = p.Vert.P
				heap.Push(mheap, graphs.Dist{Vert: graphs.FromPoint(n), Dist: w})
			}
		}
	}
	return

}

func StartCheatingWithTeleport(b [][]byte, distances [][]int, distance, maxCheat int, start, end [2]int) map[[2]int]int {
	teleports := TeleportMaxDistance(start, b, maxCheat)
	result := make(map[[2]int]int)
	for _, k := range teleports {
		newStart := k.First()
		startDist := k.Second()
		r := distances[newStart[0]][newStart[1]]
		fullDist := r + startDist + distance
		result[newStart] = fullDist
	}
	return result
}

func PartIIBFS(b [][]byte, start, end [2]int, maxCheat int, functor Counter) int {
	queue := list.New()
	queue.PushBack(start)
	mx, my := len(b), len(b[0])
	visited := make(map[[2]int]bool)
	distances, _ := DijkstraDistance(b, start, end)   // This one took some time to realize, but once you know, that the
	distancesII, _ := DijkstraDistance(b, end, start) // Graph is unoriented it is quite simple - the distance from
	base := distances[end[0]][end[1]]                 // beggining to end is the same as the other way around.
	found := make(map[[2][2]int]int)                  // So then you have the distances to teleport, distance of teleport
	res := 0                                          // And distance after teleport without any need for computation.
	for queue.Len() != 0 {                            // See the runtime difference between algI and algII printed from main.
		curr := queue.Front().Value.([2]int)
		queue.Remove(queue.Front())
		if visited[curr] {
			continue
		}
		visited[curr] = true
		desc := graphs.GenPoints(curr, mx, my)
		for _, k := range desc {
			if visited[k] || b[k[0]][k[1]] == '#' {
				continue
			}
			queue.PushBack(k)
		}
		shortCuts := StartCheatingWithTeleport(b, distancesII, distances[curr[0]][curr[1]], maxCheat, curr, end)
		for k, v := range shortCuts {
			if _, ok := found[[2][2]int{curr, k}]; !ok {
				res = functor(res, base, v)
				found[[2][2]int{curr, k}] = v
			}
		}
	}
	return res
}

func init() {
	util.SetIdentifier(20)
	flag.BoolVar(&debug, "debug", false, "Should run debug counter instead of the real one")
	flag.IntVar(&maxNo, "minSave", 100, "Filter to be used")
}
func main() {
	maze := util.InputBytes()
	start, end := graphs.FindNode(maze, 'S'), graphs.FindNode(maze, 'E')
	base, _ := graphs.Dijkstra(maze, start, end)
	fmt.Println("Shortest path in maze is", base)
	functor := RealCounter
	if debug {
		functor = DebugCounter
	}
	time1 := time.Now()
	resI := RunAllCheats(maze, start, end, base, functor)
	time2 := time.Now()
	fmt.Println("The number of cheats that would save >=100ps is", resI, "it took", time2.Sub(time1))
	if debug {
		for k, v := range debugCounter {
			fmt.Printf("There are %d cheats that save %d picoseconds\n", v, k)
		}
	}
	debugCounter = make(map[int]int)
	resICheck := PartIIBFS(maze, start, end, 2, functor)
	time3 := time.Now()
	fmt.Println("Checking new algorighm: ", resICheck, "took", time3.Sub(time2))
	if debug {
		for k, v := range debugCounter {
			fmt.Printf("There are %d cheats that save %d picoseconds\n", v, k)
		}
	}
	debugCounter = make(map[int]int)
	resII := PartIIBFS(maze, start, end, 20, functor)
	time4 := time.Now()
	fmt.Println("The number of cheats that would save >=100ps is", resII, "took", time4.Sub(time3))
	if debug {
		for k, v := range debugCounter {
			fmt.Printf("There are %d cheats that save %d picoseconds\n", v, k)
		}
	}
}
