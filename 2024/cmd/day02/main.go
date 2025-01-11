package main

import (
	"fmt"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

func IsSafePartII(c []int, r bool) bool {
	prevDiff := 0
	for i := 1; i < len(c); i++ {
		diff := c[i-1] - c[i]
		if diff == 0 || util.Abs(diff) > 3 || prevDiff*diff < 0 {
			if r {
				for i := range c {
					n := make([]int, i)
					copy(n, c)
					if i+1 < len(c) {
						n = append(n, c[i+1:]...)
					}
					if IsSafePartII(n, false) {
						return true
					}
				}
			}
			return false
		}
		prevDiff = diff
	}
	return true
}
func IsSafePartI(c []int) bool {
	prevDiff := 0
	for i := 1; i < len(c); i++ {
		diff := c[i-1] - c[i]
		if diff == 0 || util.Abs(diff) > 3 || prevDiff*diff < 0 {
			return false
		}
		prevDiff = diff
	}
	return true
}

func CountSafe(m [][]int) (int, int) {
	r1, r2 := 0, 0
	for _, v := range m {
		if IsSafePartI(v) {
			r1++
		}

		if IsSafePartII(v, true) {
			r2++
		}

	}
	return r1, r2
}

func init() {
	util.SetIdentifier(2)
}

func main() {
	mat := util.InputMatInt()
	safe1, safe2 := CountSafe(mat)
	fmt.Printf("Number of safe reports old way is %d\n", safe1)
	fmt.Printf("Number of safe reports new way is %d\n", safe2)
}
