package main

import (
	"fmt"
	"log"
	"math"
	"strconv"
	"strings"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

func exp(width int) int {
	res := 1
	for i := 0; i < width; i++ {
		res *= 10
	}
	return res
}

func oneBlink(m map[int]int) map[int]int {
	res := make(map[int]int)
	for i, j := range m {
		if i == 0 {
			res[1] += j
		} else if (int(math.Log10(float64(i)))+1)%2 == 0 {
			width := int(math.Log10(float64(i))) + 1
			newWidth := exp(width / 2)
			res[i/newWidth] += j
			res[i%newWidth] += j
		} else {
			res[i*2024] += j
		}
	}
	return res
}

func mapSum(m map[int]int) int {
	res := 0
	for _, v := range m {
		res += v
	}
	return res
}

func InLineInts(line string) []int {
	l := strings.TrimSpace(line)
	digs := strings.Split(l, " ")
	res := make([]int, len(digs))
	var err error
	for i := range res {
		res[i], err = strconv.Atoi(digs[i])
		if err != nil {
			log.Panic(err)
		}
	}
	return res
}

func blinkTimes(stones []int, count int) map[int]int {
	m := make(map[int]int)
	for _, j := range stones {
		if _, ok := m[j]; !ok {
			m[j] = 0
		}
		m[j]++
	}
	for i := 0; i < count; i++ {
		m = oneBlink(m)
	}
	return m
}

func main() {
	stones := InLineInts(util.GetInput())
	newStones := blinkTimes(stones, 25)
	fmt.Printf("Number of stones after 25 blinks is %d\n", mapSum(newStones))
	nextStones := blinkTimes(stones, 75)
	fmt.Printf("Number of stones after 75 blinks is %d\n", mapSum(nextStones))
}
