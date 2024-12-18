package main

import (
	"container/list"
	"fmt"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

type Dir byte

const (
	UP Dir = iota
	DOWN
	LEFT
	RIGHT
)

func (d Dir) Move(x, y int) (int, int) {
	switch d {
	case UP:
		return x, y - 1
	case DOWN:
		return x, y + 1
	case LEFT:
		return x - 1, y
	case RIGHT:
		return x + 1, y
	}
	return -1, -1
}

func (d Dir) Horizontal() bool {
	return d == LEFT || d == RIGHT
}

func (d Dir) Vertical() bool {
	return d == UP || d == DOWN
}

func printBoard(board [][]byte, i int) {
	fmt.Println("Board after", i)
	for _, s := range board {
		fmt.Println(string(s))
	}

}

type point struct {
	x, y int
}

func (p point) unpack() (int, int) {
	return p.x, p.y
}

func readDir(b byte) Dir {
	switch b {
	case '<':
		return LEFT
	case '>':
		return RIGHT
	case 'v':
		return DOWN
	case '^':
		return UP
	}
	return UP
}

func readDirs(pr [][]byte) []Dir {
	var r []Dir
	for _, v := range pr {
		for _, b := range v {
			r = append(r, readDir(b))
		}
	}
	return r
}

func findRobot(b [][]byte) (x, y int) {
	x, y = -1, -1
	for y = range b {
		for x = range b[y] {
			if b[y][x] == '@' {
				return
			}
		}
	}
	return
}
func procMap(plan [][]byte) int {
	var board [][]byte
	var prog []Dir

	for i := range plan {
		if string(plan[i]) == "" {
			board = plan[:i]
			prog = readDirs(plan[i+1:])
			break
		}
	}
	rx, ry := findRobot(board)
	for i := range prog {
		queue := list.New()
		queue.PushBack(point{x: rx, y: ry})
		nx, ny := prog[i].Move(rx, ry)
		for board[ny][nx] != '#' {
			if board[ny][nx] == '.' {
				break
			}
			queue.PushBack(point{x: nx, y: ny})
			nx, ny = prog[i].Move(nx, ny)
		}
		if board[ny][nx] == '#' {
			continue
		}
		nex, ney := nx, ny
		back := queue.Back()
		for back != nil {
			nx, ny = nex, ney
			nex, ney = back.Value.(point).unpack()
			back = back.Prev()
			board[ny][nx], board[ney][nex] = board[ney][nex], board[ny][nx]
		}
		rx, ry = nx, ny
	}

	sum := 0
	for i := range board {
		for j := range board[i] {
			if board[i][j] == 'O' {
				sum += 100*i + j
			}
		}
	}
	return sum

}
func horizontalMove(board [][]byte, d Dir, rx, ry int) (int, int) {
	queue := list.New()
	queue.PushBack(point{x: rx, y: ry})
	nx, ny := d.Move(rx, ry)
	for board[ny][nx] != '#' {
		if board[ny][nx] == '.' {
			break
		}
		queue.PushBack(point{x: nx, y: ny})
		nx, ny = d.Move(nx, ny)
	}
	if board[ny][nx] == '#' {
		return rx, ry
	}
	nex, ney := nx, ny
	back := queue.Back()
	for back != nil {
		nx, ny = nex, ney
		nex, ney = back.Value.(point).unpack()
		back = back.Prev()
		board[ny][nx], board[ney][nex] = board[ney][nex], board[ny][nx]
	}
	return nx, ny
}

func verticalMove(board [][]byte, d Dir, rx, ry int) (int, int) {
	queue := list.New()
	queue.PushBack([]point{{x: rx, y: ry}})
	cont := true
	for cont {
		curr := queue.Back().Value.([]point)
		next := make([]point, 0)
		set := make(map[point]bool)
		for k := range curr {
			cx, cy := curr[k].unpack()
			nx, ny := d.Move(cx, cy)
			if board[ny][nx] == '#' {
				return rx, ry
			}
			if board[ny][nx] == '[' {
				if set[point{x: nx, y: ny}] {
					continue
				}
				p1, p2 := point{x: nx, y: ny}, point{x: nx + 1, y: ny}
				next = append(next, p1, p2)
				set[p1] = true
				set[p2] = true

			}
			if board[ny][nx] == ']' {
				if set[point{x: nx, y: ny}] {
					continue
				}
				p1, p2 := point{x: nx, y: ny}, point{x: nx - 1, y: ny}
				next = append(next, p1, p2)
				set[p1] = true
				set[p2] = true

			}
		}
		if len(next) == 0 {
			cont = false
		}
		queue.PushBack(next)
	}
	back := queue.Back()
	nx, ny := rx, ry
	for back != nil {
		proc := back.Value.([]point)
		back = back.Prev()
		for _, p := range proc {
			cx, cy := p.unpack()
			nx, ny = d.Move(cx, cy)
			board[ny][nx], board[cy][cx] = board[cy][cx], board[ny][nx]
		}
	}
	return nx, ny
}

func procMapII(plan [][]byte) int {
	var board [][]byte
	var prog []Dir

	for i := range plan {
		if string(plan[i]) == "" {
			board = plan[:i]
			prog = readDirs(plan[i+1:])
			break
		}
	}
	board = transFormMapII(plan)
	rx, ry := findRobot(board)
	for i := range prog {
		if prog[i].Vertical() {
			rx, ry = verticalMove(board, prog[i], rx, ry)
		} else {
			rx, ry = horizontalMove(board, prog[i], rx, ry)
		}
	}

	sum := 0
	for i := range board {
		for j := range board[i] {
			if board[i][j] == '[' {
				sum += 100*i + j
			}
		}
	}
	return sum

}

func transFormMapII(in [][]byte) [][]byte {
	res := make([][]byte, len(in))
	for k := range in {
		res[k] = make([]byte, 2*len(in[k]))
		for i := range in[k] {
			switch in[k][i] {
			case '.':
				res[k][2*i] = '.'
				res[k][2*i+1] = '.'
			case '#':
				res[k][2*i] = '#'
				res[k][2*i+1] = '#'
			case 'O':
				res[k][2*i] = '['
				res[k][2*i+1] = ']'
			case '@':
				res[k][2*i] = '@'
				res[k][2*i+1] = '.'
			}
		}
	}
	return res
}

func main() {
	mat := util.InputBytes()
	matII := util.CopyMat(mat)
	sum := procMap(mat)
	fmt.Printf("Sum of all gps location is %d\n", sum)
	sumII := procMapII(matII)
	fmt.Printf("Sum of gps location on larger map is %d\n", sumII)
}
