package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

var num, modulus, reminder, result *int

func init() {
	util.SetIdentifier(14)
	num = flag.Int("num", 1000, "Number of pictures to generate")
	modulus = flag.Int("mod", 0, "Your found modulus")
	reminder = flag.Int("rem", 0, "your found reminder")
	result = flag.Int("res", 0, "Your partII result")
}

type Robot struct {
	x, y   int
	vx, vy int
}

func ParseRobot(l string) Robot {
	var x, y, vx, vy int
	fmt.Sscanf(l, "p=%d,%d v=%d,%d", &x, &y, &vx, &vy)
	return Robot{x: x, y: y, vx: vx, vy: vy}
}

func (r Robot) Move(t int, w, h int) Robot {
	nx := ((r.x + r.vx*t) % w)
	if nx < 0 {
		nx += w
	}
	ny := (r.y + r.vy*t) % h
	if ny < 0 {
		ny += h
	}
	r.x = nx
	r.y = ny
	return r
}

func ShowRobots(robots []Robot, w, h int) []string {
	res := make([][]byte, h)
	for k := range res {
		res[k] = make([]byte, w)
		for j := range res[k] {
			res[k][j] = '.'
		}
	}
	for _, r := range robots {
		if res[r.y][r.x] == '.' {
			res[r.y][r.x] = '1'
			continue
		}
		res[r.y][r.x]++
	}
	sres := make([]string, h)
	for k := range sres {
		sres[k] = string(res[k])
	}
	return sres
}

func countQuadrants(w, h int, rob []Robot) int {
	mw := w / 2
	mh := h / 2
	fmt.Println(mw, mh)
	quads := [4]int{0, 0, 0, 0}

	for _, k := range rob {
		if k.x < mw && k.y < mh {
			quads[0]++
		}
		if k.x > mw && k.y < mh {
			quads[1]++
		}
		if k.x < mw && k.y > mh {
			quads[2]++
		}
		if k.x > mw && k.y > mh {
			quads[3]++
		}
	}
	return quads[0] * quads[1] * quads[2] * quads[3]
}

func printRobots(outFile string, robs []Robot, w, h int) error {
	out, err := os.Create(outFile)
	if err != nil {
		fmt.Println("Couldn't write file", err)
		return err

	}
	defer out.Close()

	outD := ShowRobots(robs, w, h)
	for _, k := range outD {
		_, err = fmt.Fprintln(out, k)
		if err != nil {
			return err
		}
	}
	return nil
}

func moveRobots(r []Robot, t, w, h int) []Robot {
	res := make([]Robot, len(r))
	for k := range r {
		res[k] = r[k].Move(t, w, h)
	}
	return res
}

func writeNMoves(rob []Robot, directory string, b, s, n, w, h int) error {
	it := rob
	for i := 0; i < n; i++ {
		nextFile := fmt.Sprintf("%s/it-%d.txt", directory, b+i*s)
		err := printRobots(nextFile, it, w, h)
		if err != nil {
			return err
		}
		it = moveRobots(it, s, w, h)
	}
	return nil
}

func main() {
	rob := util.InputSlice()
	//w, h := 11, 7 //Testing value
	w, h := 101, 103 //Testing value
	robots := []Robot{}
	for k := range rob {
		robots = append(robots, ParseRobot(rob[k]))
	}
	robotMap := ShowRobots(robots, w, h)
	for _, s := range robotMap {
		fmt.Println(s)
	}
	robotsI := moveRobots(robots, 100, w, h)
	fmt.Printf("The safety factor of the quadrand is %d\n", countQuadrants(w, h, robotsI))
	if *result != 0 {
		fmt.Println("Your easter egg!")
		robotII := moveRobots(robots, *result, w, h)
		resultMap := ShowRobots(robotII, w, h)
		for _, k := range resultMap {
			fmt.Println(k)
		}
		return
	}
	if *modulus != 0 && *reminder != 0 {
		drawDirII := "day14-drawII"
		fmt.Println("This is just to verify that you were really successfull :-) use -num to limit the number")
		err := os.Mkdir(drawDirII, 0750)
		if err != nil {
			fmt.Println("Couldn't create drawing library", drawDirII)
			return
		}
		if *modulus == 0 || *reminder == 0 {
			fmt.Println("You didn't find the pattern, not continuing")
			return
		}
		robII := moveRobots(robots, *reminder, w, h)
		err = writeNMoves(robII, drawDirII, *reminder, *modulus, *num, w, h)
		return
	}

	drawDir := "day14-drawings"
	err := os.Mkdir(drawDir, 0750)
	if err != nil {
		fmt.Println("Couldn't create drawing library")
		return
	}
	err = writeNMoves(robots, drawDir, 0, 1, *num, w, h)
	if err != nil {
		fmt.Println("Couldn't write files")
	}
	fmt.Println("Go find your easter egg! (hint - you'll need linear congurencies)")
}
