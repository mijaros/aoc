package main

import (
	"fmt"
	"log"
	"strconv"
	"strings"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

type Equation struct {
	result  int
	numbers []int
}

func ShiftSum(l, r int) int {
	mul := r
	for mul > 0 {
		l *= 10
		mul /= 10
	}
	return l + r
}

func Parse(line string) Equation {
	col := strings.Split(line, ":")
	if len(col) != 2 {
		log.Panicf("Couldn't split %s", line)
	}
	res, err := strconv.Atoi(col[0])
	if err != nil {
		log.Panicln("Couldn't parse res", err)
	}
	lst := strings.Split(strings.TrimSpace(col[1]), " ")
	nums := make([]int, len(lst))
	for k := range lst {
		nums[k], err = strconv.Atoi(lst[k])
		if err != nil {
			log.Panicln(err)
		}
	}
	return Equation{result: res, numbers: nums}
}

func (e Equation) Solve() bool {
	immValues := []int{e.numbers[0]}
	operands := e.numbers[1:]

	for len(operands) > 0 {
		curr := operands[0]
		nextVal := make([]int, 0, len(immValues))
		for _, v := range immValues {
			if v*curr <= e.result {
				nextVal = append(nextVal, v*curr)
			}
			if v+curr <= e.result {
				nextVal = append(nextVal, v+curr)
			}
		}
		if len(operands) > 1 {
			operands = operands[1:]
		} else {
			operands = []int{}
		}
		immValues = nextVal
	}
	for _, v := range immValues {
		if v == e.result {
			return true
		}
	}
	return false
}

func (e Equation) SolveII() bool {
	immValues := []int{e.numbers[0]}
	operands := e.numbers[1:]

	for len(operands) > 0 {
		curr := operands[0]
		nextVal := make([]int, 0, len(immValues))
		for _, v := range immValues {
			con := ShiftSum(v, curr)
			if v*curr <= e.result {
				nextVal = append(nextVal, v*curr)
			}
			if v+curr <= e.result {
				nextVal = append(nextVal, v+curr)
			}
			if con <= e.result {
				nextVal = append(nextVal, con)
			}
		}
		if len(operands) > 1 {
			operands = operands[1:]
		} else {
			operands = []int{}
		}
		immValues = nextVal
	}
	for _, v := range immValues {
		if v == e.result {
			return true
		}
	}
	return false
}

func ParseEquations(lines []string) []Equation {
	res := make([]Equation, len(lines))
	for i := range lines {
		res[i] = Parse(lines[i])
	}
	return res
}

func init() {
	util.SetIdentifier(7)
}

func main() {
	eqString := util.InputSlice()
	equations := ParseEquations(eqString)
	sum := 0
	sumII := 0
	for _, e := range equations {
		if e.Solve() {
			sum += e.result
		}
		if e.SolveII() {
			sumII += e.result
		}

	}
	fmt.Printf("The sum of correct equations is %d\n", sum)
	fmt.Printf("The sum of correct equations PartII is %d\n", sumII)
}
