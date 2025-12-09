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
	"fmt"
	"maps"
	"slices"
	"strconv"
	"strings"

	"github.com/mijaros/adventofcode/v2025/pkg/util"
)

func init() {
	util.SetIdentifier(9)
}

type Relation int

const (
	SPLIT Relation = iota
	PREPEND
	APPEND
	CUTFRONT
	CUTBACK
	LESSER
	BIGGER
)

func parseString(s string) [2]int {
	var res [2]int
	numLit := strings.Split(s, ",")
	if len(numLit) != 2 {
		panic("Invalid line")
	}
	for i := range numLit {
		n, err := strconv.Atoi(numLit[i])
		if err != nil {
			panic(err.Error())
		}
		res[i] = n
	}
	return res
}

func InDiff(a, b int) int {
	r := a - b
	if r < 0 {
		r *= -1
	}
	return r + 1
}

type Interval struct {
	f, t int
}

func (i Interval) InInterval(p int) bool {
	return p >= i.f && p <= i.t
}

func (i Interval) GetCondition(o Interval) Relation {
	if i.f == o.t {
		return PREPEND
	}
	if i.f == o.f {
		return CUTFRONT
	}
	if i.t == o.f {
		return APPEND
	}
	if i.t == o.t {
		return CUTBACK
	}
	if o.t < i.f {
		return LESSER
	}
	if i.t < o.f {
		return BIGGER
	}
	return SPLIT
}

func BuildIntervals(points []int) []Interval {
	slices.Sort(points)
	var result []Interval
	if len(points)%2 != 0 {
		panic(fmt.Sprintf("Invalid size of points %v+", points))
	}
	for i := 0; i < len(points); i += 2 {
		result = append(result, Interval{f: points[i], t: points[i+1]})
	}
	return result
}

func decideNext(op, nex Interval, current, opened, next *[]Interval, i, j int) (int, int) {
	switch op.GetCondition(nex) {
	case LESSER:
		*next = append(*next, op)
		i++
	case BIGGER:
		*next = append((*next), nex)
		*current = append(*current, nex)
		j++
	case SPLIT:
		*next = append(*next, Interval{f: op.f, t: nex.f})
		(*opened)[i].f = nex.t
		j++
	case CUTBACK:
		(*opened)[i].t = nex.f
		j++
	case CUTFRONT:
		(*opened)[i].f = nex.t
		j++
	case PREPEND:
		(*opened)[i].f = nex.f
		(*current)[i].f = nex.f
		j++
	case APPEND:
		(*opened)[i].t = nex.t
		(*current)[i].t = nex.t
		j++
	}
	return i, j
}

func joinIntervals(i []Interval) []Interval {
	slices.SortFunc(i, func(a, b Interval) int {
		if a.f == b.f {
			return a.t - b.t
		}
		return a.f - b.f
	})
	it := i[0]
	res := make(map[Interval]bool)
	for _, v := range i[1:] {
		if it.t == v.f && v.t > it.t {
			it.t = v.t
			continue
		}
		if it.t < v.f {
			res[it] = true
			it = v
		}
	}
	res[it] = true
	return slices.Collect(maps.Keys(res))
}

func FallingLog(points [][2]int) map[int][]Interval {
	rastr := make(map[int][]int)
	result := make(map[int][]Interval)
	for _, p := range points {
		rastr[p[0]] = append(rastr[p[0]], p[1])
	}
	keys := slices.Collect(maps.Keys(rastr))
	slices.Sort(keys)
	opened := make([]Interval, 0)
	for _, k := range keys {
		var next []Interval
		current := append([]Interval{}, opened...)
		intervals := BuildIntervals(rastr[k])
		if len(opened) == 0 {
			opened = intervals
			result[k] = append(result[k], opened...)
			continue
		}
		i, j := 0, 0
		for i < len(opened) || j < len(intervals) {
			if j == len(intervals) {
				next = append(next, opened[i:]...)
				break
			}
			if i == len(opened) {
				next = append(next, intervals[j:]...)
				break
			}
			op, nex := opened[i], intervals[j]
			i, j = decideNext(op, nex, &current, &opened, &next, i, j)
		}
		result[k] = joinIntervals(append(result[k], current...))
		result[k+1] = joinIntervals(append(result[k+1], next...))
		opened = next

	}
	var curr []Interval
	maxVal := keys[len(keys)-1]
	for i := keys[0]; i <= maxVal; i++ {
		interval, ok := result[i]
		if ok {
			curr = interval
			continue
		}
		result[i] = curr
	}
	return result
}

func FindLargestRect(points [][2]int) (int, int) {
	intervals := FallingLog(points)
	if util.Verbose {
		fmt.Println(intervals)
	}
	rect1, rect2 := 0, 0
	for i := range points {
	OUT:
		for j := range points {
			if i == j {
				continue
			}
			pt1, pt2 := points[i], points[j]
			if pt1[0] > pt2[0] {
				pt1, pt2 = pt2, pt1
			}
			area := InDiff(points[i][0], points[j][0]) * InDiff(points[i][1], points[j][1])
			if area > rect1 {
				rect1 = area
			}
			for line := pt1[0]; line <= pt2[0]; line++ {
				notFound := true
				for _, interval := range intervals[line] {
					if interval.InInterval(pt1[1]) && interval.InInterval(pt2[1]) {
						notFound = false
						break
					}
				}
				if notFound {
					continue OUT
				}
			}
			if area > rect2 {
				rect2 = area
			}
		}
	}
	return rect1, rect2
}

func main() {
	data := util.InputSlice()
	indicies := util.Transform(data, parseString)
	area1, area2 := FindLargestRect(indicies)
	fmt.Println("The largest rectangular area (Part I answer) is", area1)
	fmt.Println("The largest rectangle within perimeter (Part II answer) is", area2)
}
