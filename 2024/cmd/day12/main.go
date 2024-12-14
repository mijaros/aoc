package main

import (
	"fmt"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

type Per int

const (
	UP    Per = 0b1
	DOWN  Per = 0b10
	LEFT  Per = 0b100
	RIGHT Per = 0b1000
)

func (p Per) String() string {
	switch p {
	case UP:
		return "UP"
	case DOWN:
		return "DOWN"
	case LEFT:
		return "LEFT"
	case RIGHT:
		return "RIGHT"
	}
	return ""
}

func same(i, j byte) byte {
	return i & j
}

func (p Per) Add(i byte) byte {
	return i | byte(p)
}

func extract(i byte) []Per {
	res := []Per{}
	c := 0
	for i != 0 {
		if i&1 == 1 {
			res = append(res, 1<<c)
		}
		i >>= 1
		c++
	}
	return res
}

func calcPerimeter(d [][]byte) [][]int {
	res := make([][]int, len(d))
	for k := range res {
		res[k] = make([]int, len(d[k]))
	}
	for i := range d {
		for j := range d[i] {
			if i == 0 || i+1 >= len(d) {
				res[i][j]++
			}
			if j == 0 || j+1 >= len(d[i]) {
				res[i][j]++
			}
			for cx := -1; cx < 2; cx++ {
				for cy := -1; cy < 2; cy++ {
					if i+cx < 0 || i+cx >= len(d) || j+cy < 0 || j+cy >= len(d[i]) || cx*cx == cy*cy {
						continue
					}
					if d[i+cx][j+cy] != d[i][j] {
						res[i][j]++
					}
				}
			}

		}
	}
	return res
}

func calcPerimeterII(d [][]byte) [][]int {
	per := make([][]byte, len(d))
	res := make([][]int, len(d))
	for k := range per {
		per[k] = make([]byte, len(d[k]))
		res[k] = make([]int, len(d[k]))
	}
	for i := range d {
		for j := range d[i] {
			var p byte = 0
			if i == 0 {
				p = UP.Add(p)
			}
			if i+1 == len(d) {
				p = DOWN.Add(p)
			}
			if j == 0 {
				p = LEFT.Add(p)
			}
			if j+1 >= len(d[i]) {
				p = RIGHT.Add(p)
			}
			points := genPoints([2]int{i, j}, len(d), len(d[0]))
			for _, point := range points {
				if point[0] < i && d[i][j] != d[point[0]][point[1]] {
					p = UP.Add(p)
				}
				if point[0] > i && d[i][j] != d[point[0]][point[1]] {
					p = DOWN.Add(p)
				}
				if point[1] < j && d[i][j] != d[point[0]][point[1]] {
					p = LEFT.Add(p)
				}
				if point[1] > j && d[i][j] != d[point[0]][point[1]] {
					p = RIGHT.Add(p)
				}
			}
			p2 := p
			if i > 0 && d[i-1][j] == d[i][j] {
				p2 = (p2 ^ same(p2, per[i-1][j])) & p2
			}
			if j > 0 && d[i][j-1] == d[i][j] {
				p2 = (p2 ^ same(p2, per[i][j-1])) & p2
			}
			per[i][j] = p
			res[i][j] = len(extract(p2))

		}
	}
	return res
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

func walkGraph(d [][]byte, p [][]int) [][2]int {
	visited := make(map[[2]int]bool)
	var res [][2]int
	for i := range d {
		for j := range d[0] {
			if visited[[2]int{i, j}] {
				continue
			}
			toProc := [][2]int{{i, j}}
			area, perimeter := 0, 0
			for len(toProc) > 0 {
				curr := toProc[0]
				toProc = toProc[1:]
				if visited[curr] {
					continue
				}
				points := genPoints(curr, len(d), len(d[0]))
				visited[curr] = true
				for _, p := range points {
					if visited[p] {
						continue
					}
					if d[curr[0]][curr[1]] == d[p[0]][p[1]] {
						toProc = append(toProc, p)
					}
				}
				area++
				perimeter += p[curr[0]][curr[1]]
			}
			res = append(res, [2]int{area, perimeter})
		}
	}
	return res
}

func calcArea(d [][]byte) map[byte]int {
	res := make(map[byte]int)
	for _, s := range d {
		for _, c := range s {
			res[c]++
		}
	}
	return res
}

func main() {
	dat := util.InputBytes()
	per := calcPerimeter(dat)
	components := walkGraph(dat, per)
	sum := 0
	for _, comp := range components {
		sum += comp[0] * comp[1]
	}
	fmt.Printf("The total selling price is %d\n", sum)

	perII := calcPerimeterII(dat)
	compII := walkGraph(dat, perII)
	sumII := 0
	for _, comp := range compII {
		sumII += comp[0] * comp[1]
	}
	fmt.Printf("The total selling price after optimization is %d\n", sumII)
}
