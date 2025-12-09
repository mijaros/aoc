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
	"flag"
	"fmt"
	"maps"
	"math"
	"slices"
	"strconv"
	"strings"

	"github.com/mijaros/adventofcode/v2025/pkg/util"
)

var (
	testInput bool
)

func init() {
	util.SetIdentifier(8)
	flag.BoolVar(&testInput, "test", false, "Testing input in use")
}

func Dist(a, b [3]int) float64 {
	d1, d2, d3 := a[0]-b[0], a[1]-b[1], a[2]-b[2]
	return math.Sqrt(float64(d1*d1 + d2*d2 + d3*d3))
}

func ParsePoint(s string) [3]int {
	rawNums := strings.Split(s, ",")
	if len(rawNums) != 3 {
		panic("Invalid length of line, expected 3 tokens")
	}
	var result [3]int
	for i := range rawNums {
		n, err := strconv.Atoi(rawNums[i])
		if err != nil {
			panic(err.Error())
		}
		result[i] = n
	}
	return result
}

func GetPoints() [][3]int {
	raw := util.InputSlice()
	points := make([][3]int, len(raw))
	for i := range raw {
		points[i] = ParsePoint(raw[i])
	}
	return points
}

func Distances(points [][3]int) [][]float64 {
	res := make([][]float64, len(points))
	for k := range points {
		res[k] = make([]float64, len(points))
	}

	for i := 0; i < len(res); i++ {
		for j := i + 1; j < len(res); j++ {
			if i == j {
				res[i][j] = 0
			}
			dist := Dist(points[i], points[j])
			res[i][j] = dist
			res[j][i] = dist

		}
	}
	return res
}

func PrintDist(d [][]float64) {
	for _, l := range d {
		fmt.Println(l)
	}
}

func GenPoints(dist [][]float64) [][2]int {
	res := make([][2]int, 0)
	for i := range dist {
		for j := i + 1; j < len(dist); j++ {
			res = append(res, [2]int{i, j})
		}
	}
	slices.SortFunc(res, func(l, r [2]int) int {
		if dist[l[0]][l[1]] < dist[r[0]][r[1]] {
			return -1
		}
		if dist[l[0]][l[1]] > dist[r[0]][r[1]] {
			return 1
		}
		return 0
	})
	return res
}

func stopCondition(pIndex, limit int, test, partI bool) bool {
	if test && partI {
		return pIndex < 10
	}
	if partI {
		return pIndex < 1000
	}
	return pIndex < limit
}

func Kruskal(dist [][]float64, test, partI bool) ([]*map[[2]int]bool, [2]int) {
	compMapping := make(map[int]*map[[2]int]bool)
	for k := range dist {
		m := make(map[[2]int]bool)
		compMapping[k] = &m
	}
	points := GenPoints(dist)
	pIndex := 0
	connections := 0
	lp := pIndex
	for stopCondition(pIndex, len(points), test, partI) {
		ni := points[pIndex][0]
		nj := points[pIndex][1]
		pIndex++
		cmp1 := compMapping[ni]
		cmp2 := compMapping[nj]
		if cmp1 == cmp2 {
			continue
		}
		maps.Copy(*cmp1, *cmp2)
		for k, v := range compMapping {
			if v == cmp2 {
				compMapping[k] = cmp1
			}
		}
		(*cmp1)[[2]int{ni, nj}] = true
		lp = pIndex - 1
		connections++
	}
	uniqueMap := make(map[*map[[2]int]bool]bool)
	for _, v := range compMapping {
		uniqueMap[v] = true
	}
	return slices.Collect(maps.Keys(uniqueMap)), points[lp]
}

func main() {
	junctions := GetPoints()
	distances := Distances(junctions)
	if util.Verbose {
		PrintDist(distances)
	}
	comps, _ := Kruskal(distances, testInput, true)
	ls := make([]int, 0)
	for _, v := range comps {
		ls = append(ls, len(*v)+1)
	}
	slices.SortFunc(ls, func(a, b int) int { return b - a })
	prod := 1
	for i := 0; i < 3; i++ {
		prod *= ls[i]
	}

	_, last := Kruskal(distances, testInput, false)
	fmt.Println("Product of sizes of components (Part I answer) is:", prod)
	fmt.Println("Product of last added junction (Part II answer) is:", junctions[last[0]][0]*junctions[last[1]][0])
}
