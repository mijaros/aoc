// Copyright (c) 2025 Miroslav Jaros
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// SPDX-License-Identifier: MIT

package main

import (
	"container/list"
	"fmt"
	"strings"

	"github.com/mijaros/adventofcode/v2025/pkg/util"
)

func init() {
	util.SetIdentifier(11)
}

type Graph struct {
	nodes    map[string]int
	revNodes []string
	assoc    map[int]map[int]bool
}

func (g *Graph) InsertNode(node string) int {
	p, ok := g.nodes[node]
	if ok {
		return p
	}
	p = len(g.revNodes)
	g.revNodes = append(g.revNodes, node)
	g.nodes[node] = p
	g.assoc[p] = make(map[int]bool)
	return p
}

func (g *Graph) InsertConnection(from string, to []string) {
	fId := g.InsertNode(from)
	for _, t := range to {
		tId := g.InsertNode(t)
		g.assoc[fId][tId] = true
	}
}

func NewGraph() *Graph {
	g := Graph{
		nodes:    make(map[string]int),
		revNodes: make([]string, 0),
		assoc:    make(map[int]map[int]bool)}
	return &g
}

func ParseConnection(s string) (from string, to []string) {
	mainDif := strings.Split(s, ": ")
	from = mainDif[0]
	to = strings.Split(mainDif[1], " ")
	return
}

func (g *Graph) recFindPaths(from, to int, paths map[int]int) int {
	counter := 0
	c, ok := paths[from]
	if ok {
		return c
	}
	for k, v := range g.assoc[from] {
		if k == to {
			counter += 1
			continue
		}
		if v {
			counter += g.recFindPaths(k, to, paths)
		}
	}
	paths[from] = counter
	return counter
}

func (g *Graph) FindPaths(from, to string) int {
	fId, fok := g.nodes[from]
	tId := g.nodes[to]
	if !fok {
		fmt.Println("Source node not found, not computing")
		return -1
	}
	return g.recFindPaths(fId, tId, map[int]int{})
}

func (g *Graph) PartII() int {
	src, sok := g.nodes["svr"]
	dac, dok := g.nodes["dac"]
	fft, fok := g.nodes["fft"]
	out := g.nodes["out"]
	if !sok || !dok || !fok {
		fmt.Println("Can't find required nodes, not computing")
		return -1
	}
	dacPath := g.recFindPaths(src, dac, map[int]int{})
	dacPath *= g.recFindPaths(dac, fft, map[int]int{})
	dacPath *= g.recFindPaths(fft, out, map[int]int{})

	fftPath := g.recFindPaths(src, fft, map[int]int{})
	fftPath *= g.recFindPaths(fft, dac, map[int]int{})
	fftPath *= g.recFindPaths(dac, out, map[int]int{})

	return dacPath + fftPath
}

func (g *Graph) IsAcyclicKhan() bool {
	inDeg := make([]int, len(g.revNodes))
	notFound := make(map[int]bool)
	for a, b := range g.assoc {
		notFound[a] = true
		for n, ok := range b {
			if !ok {
				continue
			}
			inDeg[n]++
		}
	}

	queue := list.New()
	for i, v := range inDeg {
		if v == 0 {
			queue.PushBack(i)
		}
	}

	for queue.Len() > 0 {
		el := queue.Front()
		node := el.Value.(int)
		queue.Remove(el)
		delete(notFound, node)
		for k := range g.assoc[node] {
			inDeg[k]--
			if inDeg[k] == 0 {
				queue.PushBack(k)
			}
		}
	}

	return len(notFound) == 0

}

func main() {
	raw := util.InputSlice()
	graph := NewGraph()
	for _, r := range raw {
		node, conn := ParseConnection(r)
		graph.InsertConnection(node, conn)
	}
	isAcyclic := graph.IsAcyclicKhan()
	fmt.Printf("Verify the graph is acyclic: \"%v\"\n", isAcyclic)
	if !isAcyclic {
		fmt.Println("Graph is not acyclic, the algorithm won't work, ending now!")
		return
	}
	p1 := graph.FindPaths("you", "out")
	if p1 != -1 {
		fmt.Println("Number of individual paths thorugh graph (Part I answer) is", p1)
	}
	p2 := graph.PartII()
	if p2 != -1 {
		fmt.Println("Number of paths going from svr to out passing through \"dac\" and \"fft\" (Part II answer) is", p2)
	}
}
