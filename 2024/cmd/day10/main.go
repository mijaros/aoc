package main

import (
	"fmt"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

type coord struct {
	x, y int
}

func findHeads(mat [][]byte) map[coord]bool {
	res := make(map[coord]bool)
	for i := range mat {
		for j := range mat[i] {
			if mat[i][j] == '0' {
				c := coord{x: i, y: j}
				res[c] = true
			}
		}
	}
	return res
}

func validCord(dx, dy int, c coord) bool {
	return c.x >= 0 && c.y >= 0 && c.x < dx && c.y < dy
}

func neighbors(mat [][]byte, c coord) []coord {
	res := make([]coord, 0, 4)
	dx, dy := len(mat), len(mat[0])
	for i := -1; i <= 1; i++ {
		for j := -1; j <= 1; j++ {
			if i != 0 && j != 0 {
				continue
			}
			c1 := coord{x: c.x + i, y: c.y + j}
			if validCord(dx, dy, c1) {
				res = append(res, c1)
			}
		}
	}
	return res
}

func doTrail(mat [][]byte, c coord) int {
	queue := map[coord]bool{c: true}
	lvl := 0
	for lvl < 9 {
		newQueue := make(map[coord]bool)
		for el, ex := range queue {
			if !ex {
				continue
			}
			neigh := neighbors(mat, el)
			for _, k := range neigh {
				if mat[k.x][k.y]-mat[el.x][el.y] == 1 {
					newQueue[k] = true
				}
			}
		}
		queue = newQueue
		lvl++
	}
	return len(queue)
}

func doTrailII(mat [][]byte, c coord) int {
	queue := []coord{c}
	lvl := 0
	for lvl < 9 {
		newQueue := make([]coord, 0)
		for _, el := range queue {
			neigh := neighbors(mat, el)
			for _, k := range neigh {
				if mat[k.x][k.y]-mat[el.x][el.y] == 1 {
					newQueue = append(newQueue, k)
				}
			}
		}
		queue = newQueue
		lvl++
	}
	return len(queue)
}

func findTrails(mat [][]byte) int {
	queue := findHeads(mat)
	res := 0

	for c, ex := range queue {
		if !ex {
			continue
		}
		res += doTrail(mat, c)
	}
	return res
}

func findTrailsII(mat [][]byte) int {
	queue := findHeads(mat)
	res := 0

	for c, ex := range queue {
		if !ex {
			continue
		}
		res += doTrailII(mat, c)
	}
	return res
}

func init() {
	util.SetIdentifier(10)
}

func main() {
	m := util.InputBytes()
	fmt.Printf("Sum of trail socres is %d\n", findTrails(m))
	fmt.Printf("Sum of all trailhead scores (with paralles is %d\n", findTrailsII(m))
}
