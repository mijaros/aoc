package graphs

import (
	"container/heap"
	"fmt"
	"math"
)

type Vertex struct {
	P      [2]int
	Parent [2]int
	Turn   bool
}

type Dist struct {
	Vert    Vertex
	Dist    int
	Parents [][2]int
	Visited map[[2]int]bool
}

func (v *Vertex) Type(p Vertex) {
	v.Turn = v.P[0] != p.P[0] && v.P[1] != p.P[1]
}

func FromPoint(p [2]int) Vertex {
	return Vertex{P: p, Turn: false}
}

type DistHeap []Dist

func (d DistHeap) Len() int {
	return len(d)
}

func (d DistHeap) Less(i, j int) bool {
	return d[i].Dist < d[j].Dist
}

func (d DistHeap) Swap(i, j int) {
	d[i], d[j] = d[j], d[i]
}

func (d *DistHeap) Push(x any) {
	*d = append(*d, x.(Dist))
}

func (d *DistHeap) Pop() any {
	ol := *d
	n := len(ol)
	r := ol[n-1]
	*d = ol[:n-1]
	return r
}

func GenPoints(p [2]int, x, y int) [][2]int {
	s := [][2]int{
		{p[0] + 1, p[1]},
		{p[0] - 1, p[1]},
		{p[0], p[1] + 1},
		{p[0], p[1] - 1}}
	var r [][2]int
	for k := range s {
		if s[k][0] >= 0 && s[k][0] < x && s[k][1] >= 0 && s[k][1] < y {
			r = append(r, s[k])
		}
	}
	return r
}

func DijkstraSpec(b [][]byte) int {
	h, w := len(b), len(b[0])
	distances := GenDistance(len(b), len(b[0]), math.MaxInt)
	parents := GenDistance(len(b), len(b[0]), [2]int{-1, -1})
	start := FindNode(b, 'S')
	end := FindNode(b, 'E')
	mheap := &DistHeap{{Vert: FromPoint(start), Dist: 0}}
	distances[start[0]][start[1]] = 0
	parents[start[0]][start[1]] = [2]int{start[0], start[1] - 1}
	heap.Init(mheap)
	closed := make(map[Vertex]bool)

	for mheap.Len() != 0 {
		p := heap.Pop(mheap).(Dist)
		if closed[p.Vert] {
			continue
		}
		closed[p.Vert] = true
		neighs := GenPoints(p.Vert.P, h, w)
		par := parents[p.Vert.P[0]][p.Vert.P[1]]
		for _, n := range neighs {
			if b[n[0]][n[1]] == '#' || closed[FromPoint(n)] {
				continue
			}
			w := p.Dist + 1
			if par[0] != n[0] && par[1] != n[1] {
				w += 1000
			}
			if w < distances[n[0]][n[1]] {
				distances[n[0]][n[1]] = w
				parents[n[0]][n[1]] = p.Vert.P
				heap.Push(mheap, Dist{Vert: FromPoint(n), Dist: w})
			}
		}
	}
	return distances[end[0]][end[1]]

}

func Dijkstra(b [][]byte, start, end [2]int) (int, [][2]int) {
	h, w := len(b), len(b[0])
	distances := GenDistance(len(b), len(b[0]), math.MaxInt)
	parents := GenDistance(len(b), len(b[0]), [2]int{-1, -1})
	mheap := &DistHeap{{Vert: FromPoint(start), Dist: 0}}
	distances[start[0]][start[1]] = 0
	parents[start[0]][start[1]] = [2]int{start[0], start[1] - 1}
	heap.Init(mheap)
	closed := make(map[Vertex]bool)

	for mheap.Len() != 0 {
		p := heap.Pop(mheap).(Dist)
		if closed[p.Vert] {
			continue
		}
		closed[p.Vert] = true
		neighs := GenPoints(p.Vert.P, h, w)
		for _, n := range neighs {
			if b[n[0]][n[1]] == '#' || closed[FromPoint(n)] {
				continue
			}
			w := p.Dist + 1
			if w < distances[n[0]][n[1]] {
				distances[n[0]][n[1]] = w
				parents[n[0]][n[1]] = p.Vert.P
				heap.Push(mheap, Dist{Vert: FromPoint(n), Dist: w})
			}
		}
	}
	it := end
	path := [][2]int{end}
	for it != start {
		it = parents[it[0]][it[1]]
		path = append(path, it)
	}
	return distances[end[0]][end[1]], path

}

func PrintMat(b [][]byte) {
	for _, l := range b {
		fmt.Println(string(l))
	}
}

func GenDistance[T any](h, w int, v T) [][]T {
	res := make([][]T, h)
	for k := range res {
		res[k] = make([]T, w)
		for j := range res[k] {
			res[k][j] = v
		}
	}
	return res
}
func FindNode(b [][]byte, v byte) [2]int {
	for i, l := range b {
		for j, c := range l {
			if c == v {
				return [2]int{i, j}
			}
		}
	}
	return [2]int{-1, -1}
}
