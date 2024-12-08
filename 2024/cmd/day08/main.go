package main

import (
	"fmt"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

type Antena struct {
	feq  byte
	x, y int
}

func extractAntena(m [][]byte) map[byte][]Antena {
	res := make(map[byte][]Antena)
	for i := range m {
		for j, c := range m[i] {
			if c == '.' {
				continue
			}
			if _, ok := res[c]; !ok {
				res[c] = make([]Antena, 0)
			}
			res[c] = append(res[c], Antena{feq: c, x: i, y: j})
		}
	}
	return res
}

func validPoint(x, y, dimX, dimY int) bool {
	return x >= 0 && y >= 0 && x < dimX && y < dimY
}

func findAntinodesII(m [][]byte) ([][]byte, int) {
	locations := extractAntena(m)
	aNodes := util.CopyMat(m)
	antiNodes := 0
	dimX, dimY := len(aNodes), len(aNodes[0])

	for _, t := range locations {
		for i := 0; i < len(t)-1; i++ {
			for j := i + 1; j < len(t); j++ {
				f, s := t[i], t[j]
				x, y := f.x-s.x, f.y-s.y
				x1, x2 := f.x, s.x
				y1, y2 := f.y, s.y
				for validPoint(x1, y1, dimX, dimY) {
					if aNodes[x1][y1] != '#' {
						antiNodes++
						aNodes[x1][y1] = '#'
					}
					x1 += x
					y1 += y
				}
				for validPoint(x2, y2, dimX, dimY) {
					if aNodes[x2][y2] != '#' {
						antiNodes++
						aNodes[x2][y2] = '#'
					}
					x2 -= x
					y2 -= y
				}
			}
		}
	}
	return aNodes, antiNodes
}
func findAntinodesI(m [][]byte) ([][]byte, int) {
	locations := extractAntena(m)
	aNodes := util.CopyMat(m)
	antiNodes := 0
	dimX, dimY := len(aNodes), len(aNodes[0])

	for _, t := range locations {
		for i := 0; i < len(t)-1; i++ {
			for j := i + 1; j < len(t); j++ {
				f, s := t[i], t[j]
				x, y := f.x-s.x, f.y-s.y
				x1, x2 := f.x+x, s.x-x
				y1, y2 := f.y+y, s.y-y
				if validPoint(x1, y1, dimX, dimY) && aNodes[x1][y1] != '#' {
					antiNodes++
					aNodes[x1][y1] = '#'
				}
				if validPoint(x2, y2, dimX, dimY) && aNodes[x2][y2] != '#' {
					antiNodes++
					aNodes[x2][y2] = '#'
				}

			}
		}
	}
	return aNodes, antiNodes
}

func main() {
	antenaMap := util.InputBytes()
	aMap, num := findAntinodesI(antenaMap)
	fmt.Println("=== PartI Map ===")
	for k := range aMap {
		fmt.Printf("%s \t %s\n", string(aMap[k]), string(antenaMap[k]))
	}

	aMapII, numII := findAntinodesII(antenaMap)
	fmt.Println("=== PartII Map ===")
	for k := range aMapII {
		fmt.Printf("%s \t %s\n", string(aMapII[k]), string(antenaMap[k]))
	}
	fmt.Printf("Number of antinodes is %d\n", num)
	fmt.Printf("Number of antinodes with harmonics is %d\n", numII)
}
