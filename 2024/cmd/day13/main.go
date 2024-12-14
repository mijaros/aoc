package main

import (
	"fmt"
	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

const PartIIOffset = 10000000000000

func gcd(a, b int) int {

	if a < b {
		return gcd(b, a)
	}
	if b == 0 {
		return 1
	}
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func solveEquation(ieq [][]int) []int {
	eq := util.CopyMat(ieq)
	res := make([]int, len(eq))
	for i := 0; i < len(eq)-1; i++ {
		top := eq[i][i]
		for j := i + 1; j < len(eq); j++ {
			g := gcd(top, eq[j][i])
			im, jm := top/g, eq[j][i]/g
			for k := 0; k < len(eq[j]); k++ {
				eq[j][k] = (im)*eq[j][k] - (jm)*eq[i][k]
			}
		}
	}
	for i := len(eq) - 1; i >= 0; i-- {
		if i+1 == len(eq[i]) {
			break
		}
		mr := eq[i][len(eq[i])-1]
		for j := i + 1; j < len(res); j++ {
			mr -= eq[i][j] * res[j]
		}
		g := gcd(eq[i][i], mr)
		if g != eq[i][i] {
			return []int{}
		}
		res[i] = mr / g

	}
	return res
}

func parsePoints(s []string, i int) [][]int {
	var x1, y1, x2, y2, x3, y3 int
	fmt.Sscanf(s[i], "Button A: X+%d, Y+%d\n", &x1, &y1)
	fmt.Sscanf(s[i+1], "Button B: X+%d, Y+%d\n", &x2, &y2)
	fmt.Sscanf(s[i+2], "Prize: X=%d, Y=%d\n", &x3, &y3)
	return [][]int{{x1, x2, x3}, {y1, y2, y3}}
}

func extendPartII(i [][]int) [][]int {
	res := util.CopyMat(i)
	for j := range res {
		res[j][len(res[j])-1] += PartIIOffset
	}
	return res
}

func countTokens(matrices [][][]int) int {
	res := 0
	for k := range matrices {
		mr := solveEquation(matrices[k])
		if len(mr) != 2 {
			continue
		}
		res += mr[0]*3 + mr[1]
	}
	return res
}

func main() {
	dat := util.InputSlice()
	var matrices [][][]int
	i := 0
	for i < len(dat) {
		mat := parsePoints(dat, i)
		i += 4
		matrices = append(matrices, mat)

	}
	resI := countTokens(matrices)
	fmt.Printf("Necessary number of tokens PartI is %d\n", resI)

	var matricesII [][][]int
	for k := range matrices {
		matricesII = append(matricesII, extendPartII(matrices[k]))
	}

	resII := countTokens(matricesII)
	fmt.Printf("Necessary number of tokens after correction is %d\n", resII)

}
