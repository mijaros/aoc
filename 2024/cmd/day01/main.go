package main

import (
	"fmt"
	"sort"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

func parseInput() [][]int {
	inMat := util.InputMatInt()
	result := [][]int{make([]int, len(inMat)), make([]int, len(inMat))}
	for i, v := range inMat {
		result[0][i], result[1][i] = v[0], v[1]
	}
	return result

}

func Count(val int, dat []int) (int, bool) {
	i, found := sort.Find(len(dat), func(i int) int {
		if val == dat[i] {
			return 0
		}
		if val < dat[i] {
			return -1
		}
		return 1
	})
	if !found {
		return 0, found
	}
	c := 0
	for _, v := range dat[i:] {
		if v != val {
			break
		}
		c++
	}
	return c, true
}

func SolvePartI(in [][]int) int {
	left, right := in[0], in[1]
	sort.Ints(left)
	sort.Ints(right)
	res := 0
	for i := 0; i < len(left); i++ {
		res += util.Abs(left[i] - right[i])
	}
	return res
}

func SolvePartII(in [][]int) int {
	left, right := in[0], in[1]
	sort.Ints(right)
	res := 0
	for _, v := range left {
		c, f := Count(v, right)
		if !f {
			continue
		}
		res += c * v
	}
	return res
}

func main() {
	data := parseInput()
	r := SolvePartI(data)
	fmt.Printf("Counted difference is %d\n", r)
	r2 := SolvePartII(data)
	fmt.Printf("Calculated score is %d\n", r2)
}
