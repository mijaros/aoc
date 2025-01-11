package main

import (
	"container/heap"
	"flag"
	"fmt"
	"math"
	"strconv"
	"strings"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

var (
	width *int
	bytes *int
)

type NodeHeap [][2]int

func (h NodeHeap) Len() int           { return len(h) }
func (h NodeHeap) Less(i, j int) bool { return h[i][1] < h[j][1] }
func (h NodeHeap) Swap(i, j int)      { h[i], h[j] = h[j], h[i] }

func (h *NodeHeap) Push(x any) {
	*h = append(*h, x.([2]int))
}

func (h *NodeHeap) Pop() any {
	old := *h
	n := len(old)
	x := old[n-1]
	*h = old[0 : n-1]
	return x
}

func init() {
	util.SetIdentifier(18)
	width = flag.Int("width", 71, "Width of the memory grid")
	bytes = flag.Int("bytes", 1024, "Number of bytes to fall")
}

type Node struct {
	x, y      int
	neighbors []*Node
}

func (n *Node) AddNeighbor(nei *Node) {
	n.neighbors = append(n.neighbors, nei)
	nei.neighbors = append(nei.neighbors, n)
}

func genPoints(p [2]int, x, y int) [][2]int {
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

func GridToGraph(s [][]byte) []*Node {
	set := make(map[[2]int]bool)
	nodes := make([][]*Node, len(s))
	for k := range nodes {
		nodes[k] = make([]*Node, len(s[0]))
	}
	mx, my := len(s), len(s[0])
	for i := range s {
		for j := range s[i] {
			if s[i][j] != '.' {
				continue
			}
			set[[2]int{i, j}] = true
			newNode := &Node{x: i, y: j, neighbors: []*Node{}}
			nodes[i][j] = newNode
			neighbors := genPoints([2]int{i, j}, mx, my)
			for _, n := range neighbors {
				x, y := n[0], n[1]
				if set[n] {
					newNode.AddNeighbor(nodes[x][y])
				}
			}
		}
	}
	res := make([]*Node, 0, len(s))
	for _, v := range nodes {
		for _, n := range v {
			if n == nil {
				continue
			}
			res = append(res, n)
		}
	}
	return res
}

func GenGrid(w, t int, points [][2]int) [][]byte {
	res := make([][]byte, w)
	for i := range res {
		res[i] = make([]byte, w)
		for j := range res[i] {
			res[i][j] = '.'
		}
	}
	for i := range points {
		if i >= t {
			break
		}
		res[points[i][1]][points[i][0]] = '#'
	}
	return res
}

func ParsePoints(inp []string) [][2]int {
	res := make([][2]int, len(inp))
	for i, line := range inp {
		ind := strings.Split(line, ",")
		res[i][0], _ = strconv.Atoi(ind[0])
		res[i][1], _ = strconv.Atoi(ind[1])
	}
	return res
}

func PrintMat(m [][]byte) {
	for k := range m {
		fmt.Println(string(m[k]))
	}
}

func genDistance(n int) []int {
	res := make([]int, n)
	for i := range res {
		res[i] = math.MaxInt
	}
	return res
}

func Dijkstra(graph []*Node) int {
	reverseMap := make(map[*Node]int)
	for i, n := range graph {
		reverseMap[n] = i
	}
	gheap := &NodeHeap{[2]int{0, 0}}
	heap.Init(gheap)
	d := genDistance(len(graph))
	closed := make([]bool, len(graph))
	d[0] = 0

	for gheap.Len() != 0 {
		p := heap.Pop(gheap).([2]int)
		if closed[p[0]] {
			continue
		}
		currNode := graph[p[0]]
		closed[p[0]] = true
		for _, n := range currNode.neighbors {
			ind := reverseMap[n]
			if !closed[ind] && d[p[0]]+1 < d[ind] {
				d[ind] = d[p[0]] + 1
				heap.Push(gheap, [2]int{ind, d[ind]})
			}
		}
	}
	return d[len(graph)-1]
}

func main() {
	points := ParsePoints(util.InputSlice())
	grid := GenGrid(*width, *bytes, points)
	graph := GridToGraph(grid)
	fmt.Println("The distance of the path is", Dijkstra(graph))
	for i := *bytes; i < len(points); i++ {
		tmpGrid := GenGrid(*width, i+1, points)
		tmpGraph := GridToGraph(tmpGrid)
		if Dijkstra(tmpGraph) == math.MaxInt {
			fmt.Printf("The breaking byte is at index [%d,%d]\n", points[i][0], points[i][1])
			break
		}
	}
}
