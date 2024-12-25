package main

import (
	"errors"
	"fmt"
	"log"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

type Direction byte

const (
	NORTH Direction = iota
	EAST
	SOUTH
	WEST
)

func (d Direction) Turn() Direction {
	return (d + 1) % 4
}

func (d Direction) MapRere() byte {
	switch d {
	case NORTH:
		return '^'
	case EAST:
		return '>'
	case SOUTH:
		return 'v'
	case WEST:
		return '<'
	}

	return '.'
}

func ReadDirection(b byte) (Direction, error) {
	switch b {
	case '^':
		return NORTH, nil
	case '>':
		return EAST, nil
	case 'v':
		return SOUTH, nil
	case '<':
		return WEST, nil
	}
	return NORTH, errors.New("Invalid direction")
}

type Guard struct {
	x, y int
	dir  Direction
}

func (g Guard) Advance() Guard {
	switch g.dir {
	case NORTH:
		g.x--
	case EAST:
		g.y++
	case SOUTH:
		g.x++
	case WEST:
		g.y--
	}
	return g
}

func (g Guard) Turn() Guard {
	g.dir = g.dir.Turn()
	return g
}

func (g Guard) MapR() byte {
	return g.dir.MapRere()
}

func FindGuard(m [][]byte) (Guard, error) {
	for i := range m {
		for j := range m[i] {
			d, err := ReadDirection(m[i][j])
			if err == nil {
				return Guard{x: i, y: j, dir: d}, nil
			}
		}
	}
	return Guard{}, errors.New("Couldn't find Guard on the map")
}

func WalkMap(in [][]byte) ([][]byte, bool) {
	m := make([][]byte, len(in))
	for k := range in {
		m[k] = make([]byte, len(in[k]))
		copy(m[k], in[k])
	}
	guard, err := FindGuard(m)
	if err != nil {
		log.Panic(err)
	}

	guardPos := make(map[Guard]bool)
	dx, dy := len(m), len(m[0])

	for !guardPos[guard] {
		guardPos[guard] = true
		nextPos := guard.Advance()
		i := 0
		for nextPos.x < 0 || nextPos.y < 0 || nextPos.x >= dx || nextPos.y >= dy || m[nextPos.x][nextPos.y] == '#' {
			if nextPos.x < 0 || nextPos.y < 0 || nextPos.x >= dx || nextPos.y >= dy {
				m[guard.x][guard.y] = 'X'
				return m, false
			}
			if i > 4 {

				log.Panic("Indefinite loop detected", guard, nextPos)
			}
			i++
			guard = guard.Turn()
			nextPos = guard.Advance()
		}
		m[guard.x][guard.y] = 'X'
		m[nextPos.x][nextPos.y] = nextPos.MapR()
		guard = nextPos
	}
	return m, true
}
func CountPos(m [][]byte) int {
	res := 0
	for _, l := range m {
		for _, c := range l {
			if c == 'X' {
				res++
			}
		}
	}
	return res
}

func FindLooping(m [][]byte) int {
	res := 0
	for i := range m {
		for j := range m[i] {
			if m[i][j] != '.' {
				continue
			}
			m[i][j] = '#'
			if _, loop := WalkMap(m); loop {
				res++
			}
			m[i][j] = '.'
		}
	}
	return res
}

func main() {
	mat := util.InputBytes()

	m2, _ := WalkMap(mat)
	if util.Verbose {
		fmt.Println("Solution")
		for k := range m2 {
			fmt.Println(string(m2[k]))
		}
	}
	fmt.Printf("Number of visited places is %d\n", CountPos(m2))
	fmt.Printf("Number of possible loops is %d\n", FindLooping(mat))
}
