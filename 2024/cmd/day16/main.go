package main

import (
	"container/heap"
	"fmt"
	"maps"

	"github.com/mijaros/adventofcode/v2024/pkg/graphs"
	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

func markJunctions(b [][]byte) [][]byte {
	w, h := len(b), len(b[0])
	for i := range b {
		for j := range b[i] {
			if b[i][j] != '.' {
				continue
			}
			lw := graphs.GenPoints([2]int{i, j}, w, h)
			count := 0
			for _, p := range lw {
				if b[p[0]][p[1]] == '.' {
					count++
				}
			}
			if count > 2 {
				b[i][j] = 'J'
			}
		}
	}
	return b
}

func printMat(b [][]byte) {
	for _, l := range b {
		fmt.Println(string(l))
	}
}

func printParents(b [][]map[[2]int]bool) {
	for _, l := range b {
		for j, c := range l {
			if j != 0 {
				fmt.Printf(" ")
			}
			fmt.Printf("%d", len(c))
		}
		fmt.Println()
	}
}

func BackTrack(paths [][][2]int, b [][]byte) int {
	visited := make(map[[2]int]bool)
	for _, path := range paths {
		for _, point := range path {
			if b[point[0]][point[1]] != '#' {
				b[point[0]][point[1]] = 'O'
				visited[point] = true
			}
		}
	}
	if util.Verbose {
		printMat(b)
	}
	return len(visited)
}

// This is evil function which allows parallels in dijstras execution
// (see no distances vector) and also redefines what is a vertex
// In order to distinguish the nodes where turn happened, the vertex definition
// is extended by it's parent - so basically from the algorithm standpoint,
// vertex visited from below and from right are two distinct vertices even though
// it is on the same spot on the map
// reason being this situation  #.#
//
//	..#
//	#.#
//
// where the point in the middle (junction) can have the distance from the beginning
// 1001 by the left path, and 2001 from the down path - hence the dijsktras algorithm
// would completely skip the lower path, but the top point has a distance of
// 2002 nevertheless the path that was taken from the point below (because if the path is
// from left, there is turn necessary.
func DijkstraWithParallels(b [][]byte, start, end [2]int, distance int) [][][2]int {
	h, w := len(b), len(b[0])
	mheap := &graphs.DistHeap{{Vert: graphs.FromPoint(start), Dist: 0, Parents: [][2]int{{start[0], start[1] - 1}}, Visited: make(map[[2]int]bool)}}
	heap.Init(mheap)
	result := make([][][2]int, 0)
	visited := make(map[graphs.Vertex]bool)
	for mheap.Len() != 0 {
		curr := heap.Pop(mheap).(graphs.Dist)
		if curr.Vert.P == end && curr.Dist == distance {
			result = append(result, append(curr.Parents, curr.Vert.P))
			continue
		}
		curr.Visited[curr.Vert.P] = true
		visited[curr.Vert] = true
		neighs := graphs.GenPoints(curr.Vert.P, h, w)
		for _, n := range neighs {
			if b[n[0]][n[1]] == '#' {
				continue
			}
			if curr.Visited[n] {
				continue
			}
			d := curr.Dist + 1
			parent := util.Last(curr.Parents)
			nv := graphs.FromPoint(n)
			nv.Type(graphs.FromPoint(*parent))
			nv.Parent = curr.Vert.P
			if (*parent)[0] != n[0] && (*parent)[1] != n[1] {
				d += 1000
			}
			if d > distance {
				continue
			}
			if visited[nv] {
				continue
			}
			newDist := graphs.Dist{
				Vert:    nv,
				Dist:    d,
				Visited: maps.Clone(curr.Visited),
				Parents: append(append([][2]int{}, curr.Parents...), curr.Vert.P)}
			heap.Push(mheap, newDist)
		}

	}
	return result

}
func main() {
	inp := util.InputBytes()
	dist := graphs.DijkstraSpec(inp)
	start, end := graphs.FindNode(inp, 'S'), graphs.FindNode(inp, 'E')
	paths := DijkstraWithParallels(inp, start, end, dist)
	tiles := BackTrack(paths, inp)
	fmt.Printf("The lowest reindeer score is %d\n", dist)
	fmt.Printf("The number of tiles is %d\n", tiles)
}
