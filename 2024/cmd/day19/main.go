package main

import (
	"fmt"
	"strings"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

func extractTowels(line string) []string {
	return strings.Split(line, ", ")
}

func compose(towel string, materials []string) int {
	suffixes := make(map[string]int)
	suffixes[towel] = 1
	result := 0
	for len(suffixes) != 0 {
		nextSuf := make(map[string]int)
		for suf, num := range suffixes {
			for _, m := range materials {
				p, c := strings.CutPrefix(suf, m)
				if !c {
					continue
				}
				if p == "" {
					result += num
					continue
				}
				nextSuf[p] += num
			}
		}
		suffixes = nextSuf
	}
	return result
}

func main() {
	input := util.InputSlice()
	towels := extractTowels(input[0])
	newTowels := input[2:]
	p1 := 0
	p2 := 0
	for k := range newTowels {
		bills := compose(newTowels[k], towels)
		if bills != 0 {
			p1++
			p2 += bills
		}
	}
	fmt.Printf("Number of arrangable towels is %d\n", p1)
	fmt.Printf("Number of all possible arrangements for towels is %d\n", p2)
}
