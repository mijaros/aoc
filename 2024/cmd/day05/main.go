package main

import (
	"fmt"
	"log"
	"math"
	"strconv"
	"strings"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

type DependencyGraph struct {
	dependsOn, isDependent map[int]map[int]bool
}

func (d *DependencyGraph) AddDep(from, to int) {
	if _, ok := d.dependsOn[from]; !ok {
		d.dependsOn[from] = make(map[int]bool)
	}
	if _, ok := d.isDependent[to]; !ok {
		d.isDependent[to] = make(map[int]bool)
	}
	d.dependsOn[from][to] = true
	d.isDependent[to][from] = true
}

func (d *DependencyGraph) CorrectPlan(plan []int) bool {
	visited := make(map[int]bool)
	for _, v := range plan {
		deps := d.dependsOn[v]
		for d, ok := range visited {
			if !ok {
				continue
			}
			if deps[d] {
				return false
			}
		}
		visited[v] = true
	}
	return true
}

func findMin(deps map[int]int) int {
	minD, mk := math.MaxInt, -1
	for k, v := range deps {
		if v < minD {
			minD = v
			mk = k
		}
	}
	return mk
}

func (d *DependencyGraph) FixPlan(plan []int) []int {
	set := make(map[int]bool)
	in_deg := make(map[int]int)
	for _, k := range plan {
		set[k] = true
	}

	for k := range set {
		pre := d.isDependent[k]
		incident := 0
		for v := range pre {
			if set[v] {
				incident++
			}
		}
		in_deg[k] = incident
	}
	result := make([]int, len(plan))
	k := 0
	for len(in_deg) != 0 {
		curr := findMin(in_deg)
		if in_deg[curr] != 0 {
			log.Println("Something is wrong with the set", in_deg)
		}
		result[k] = curr
		k++
		for child := range d.dependsOn[curr] {
			if _, ok := in_deg[child]; ok {
				in_deg[child]--
			}
		}
		delete(in_deg, curr)
	}
	return result
}

func NewGraph() *DependencyGraph {
	g := DependencyGraph{dependsOn: make(map[int]map[int]bool), isDependent: make(map[int]map[int]bool)}
	return &g
}

func ExtractOrder(in []string) (n int, graph *DependencyGraph) {
	n, graph = 0, NewGraph()
	for n = range in {
		if in[n] == "" {
			n++
			return
		}
		rules := strings.Split(in[n], "|")
		iRules := [2]int{}
		if len(rules) != 2 {
			log.Panicln("Invalid string", in[n])
		}
		for k := range rules {
			l, err := strconv.Atoi(rules[k])
			if err != nil {
				log.Panicln(err)
			}
			iRules[k] = l
		}
		graph.AddDep(iRules[0], iRules[1])

	}
	return
}

func ExtractPrintRules(in []string) [][]int {
	result := make([][]int, len(in))
	for k := range result {
		strs := strings.Split(in[k], ",")
		result[k] = make([]int, len(strs))
		for j := range result[k] {
			v, err := strconv.Atoi(strs[j])
			if err != nil {
				log.Panic(err)
			}
			result[k][j] = v
		}
	}
	return result
}

func main() {
	s := util.InputSlice()
	n, graph := ExtractOrder(s)
	rules := ExtractPrintRules(s[n:])
	result := 0
	resultFixed := 0
	for _, rule := range rules {
		if graph.CorrectPlan(rule) {
			result += rule[len(rule)/2]
		} else {
			newRule := graph.FixPlan(rule)
			resultFixed += newRule[len(newRule)/2]
		}
	}
	fmt.Printf("Sum of page numbers is %d\n", result)
	fmt.Printf("Sum of page numbers in fixed is %d\n", resultFixed)

}
