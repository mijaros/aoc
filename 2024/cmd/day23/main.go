package main

import (
	"fmt"
	"maps"
	"slices"
	"strings"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

type Node struct {
	name      string
	neighbors map[string]*Node
}

type Graph struct {
	nodes map[string]*Node
}

func (n *Node) AddEdge(t *Node) {
	n.neighbors[t.name] = t
	t.neighbors[n.name] = n
}

func NewGraph() *Graph {
	return &Graph{nodes: make(map[string]*Node)}
}

func NewNode(name string) *Node {
	return &Node{name: name, neighbors: make(map[string]*Node)}
}

func (g *Graph) GetNode(name string) *Node {
	fNode, ok := g.nodes[name]
	if !ok {
		fNode = NewNode(name)
		g.nodes[name] = fNode
	}
	return fNode
}

func (n *Node) HasEdge(t *Node) bool {
	_, ok := n.neighbors[t.name]
	return ok
}

func (g *Graph) FindTriangles() [][]string {
	names := slices.Collect(maps.Keys(g.nodes))
	results := make([][]string, 0)
	for p, i := range names {
		for q, j := range names[p+1:] {
			for _, k := range names[p+q+2:] {
				f, s, t := g.nodes[i], g.nodes[j], g.nodes[k]
				if f.HasEdge(s) && s.HasEdge(t) && t.HasEdge(f) {
					results = append(results, []string{f.name, s.name, t.name})
				}
			}
		}
	}
	return results
}

func (g *Graph) InsertEdge(from, to string) {
	fNode := g.GetNode(from)
	tNode := g.GetNode(to)
	fNode.AddEdge(tNode)
}

func (g *Graph) ParseLine(line string) {
	r := strings.Split(line, "-")
	if len(r) != 2 {
		panic("Invalid input")
	}
	g.InsertEdge(r[0], r[1])
}

func BuildGraphFromInput(lines []string) *Graph {
	res := NewGraph()
	for _, k := range lines {
		res.ParseLine(k)
	}
	return res
}

func PrintTriangles(tri [][]string) {
	for _, k := range tri {
		for v := range k {
			if v != 0 {
				fmt.Printf(",")
			}
			fmt.Printf("%s", k[v])
		}
		fmt.Println()
	}
}

func CountTBegginning(in [][]string) int {
	res := 0
	for _, l := range in {
		for _, v := range l {
			if strings.HasPrefix(v, "t") {
				res++
				break
			}
		}
	}
	return res
}

func (g *Graph) FindClique(clique map[*Node]bool) map[*Node]bool {
	for _, v := range g.nodes {
		if clique[v] {
			continue
		}
		hasEdge := true
		for n := range clique {
			if !n.HasEdge(v) {
				hasEdge = false
				break
			}
		}
		if hasEdge {
			clique[v] = true
		}
	}
	maps.DeleteFunc(clique, func(k *Node, v bool) bool { return !v })
	return clique
}

// This function reuses the fact, that any clique contains many many triangles,
// which means, we can reuse this fact to reduce some searching time, and it could be
// even better considering, that many of these triangles will be acutally part of the same clique
// So if it wasn't fast enough (right now it is On^2 per triangle) we could salvage some time
// merging the triangles into K_n cliques and start with already pretty big cliques with reduced
// number of search tree.
func (g *Graph) FindMaxClique(triangles [][]string) []*Node {
	var res map[*Node]bool = map[*Node]bool{}
	for _, triangle := range triangles {
		ntri := make(map[*Node]bool)
		for _, v := range triangle {
			ntri[g.nodes[v]] = true
		}
		trClique := g.FindClique(ntri)
		if len(trClique) > len(res) {
			res = trClique
		}
	}
	return slices.Collect(maps.Keys(res))
}

func BuildResString(clique []*Node) string {
	names := util.Transform(clique, func(n *Node) string {
		return n.name
	})
	slices.Sort(names)
	return strings.Join(names, ",")
}

func main() {
	g := BuildGraphFromInput(util.InputSlice())
	triangles := g.FindTriangles()
	if util.Verbose {
		fmt.Println(*g)
		PrintTriangles(triangles)
	}
	count := CountTBegginning(triangles)
	fmt.Printf("The number of triangles with t beggining nodes is %d\n", count)

	mclique := g.FindMaxClique(triangles)
	fmt.Printf("The resulting clique is \"%s\"\n", BuildResString(mclique))
}
