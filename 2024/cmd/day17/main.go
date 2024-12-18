package main

import (
	"errors"
	"fmt"
	"log"
	"strconv"
	"strings"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

type Computer struct {
	regA, regB, regC int
	ip               int
	prog             []int
}

func (c *Computer) ComboOb(i int) int {
	switch i {
	case 0, 1, 2, 3:
		return i
	case 4:
		return c.regA
	case 5:
		return c.regB
	case 6:
		return c.regC
	}
	return -1
}

func (c *Computer) Adv(op int) {
	val := c.ComboOb(op)
	c.regA /= 1 << val
	c.ip += 2
}

func (c *Computer) Bxl(op int) {
	c.regB ^= op
	c.ip += 2
}

func (c *Computer) Bst(op int) {
	val := c.ComboOb(op)
	c.regB = val % 8
	c.ip += 2
}

func (c *Computer) Jnz(op int) {
	if c.regA == 0 {
		c.ip += 2
		return
	}
	c.ip = op

}

func (c *Computer) Bxc(op int) {
	c.regB ^= c.regC
	c.ip += 2
}

func (c *Computer) Out(op int) int {
	val := c.ComboOb(op) % 8
	c.ip += 2
	return val
}

func (c *Computer) Bdv(op int) {
	val := c.ComboOb(op)
	c.regB = c.regA / (1 << val)
	c.ip += 2
}

func (c *Computer) Cdv(op int) {
	val := c.ComboOb(op)
	c.regC = c.regA / (1 << val)
	c.ip += 2
}

func (c *Computer) Tick() (int, bool) {
	inst, op := c.prog[c.ip], c.prog[c.ip+1]
	res, set := 0, false
	switch inst {
	case 0:
		c.Adv(op)
	case 1:
		c.Bxl(op)
	case 2:
		c.Bst(op)
	case 3:
		c.Jnz(op)
	case 4:
		c.Bxc(op)
	case 5:
		res = c.Out(op)
		set = true
	case 6:
		c.Bdv(op)
	case 7:
		c.Cdv(op)

	}
	return res, set
}

func (c *Computer) Run() []int {
	res := make([]int, 0)
	for c.ip < len(c.prog) {
		v, set := c.Tick()
		if set {
			res = append(res, v)
		}
	}
	return res
}
func (c *Computer) RunII() int {
	res := 0
	i := 0
	for c.ip < len(c.prog) {
		v, set := c.Tick()
		if set {
			res = res | (v << (i * 3))
			i++
		}
	}
	return res
}

func (c *Computer) ReverseCalculation(aVal, i, exp int) (int, error) {
	if i == -1 {
		return 0, errors.New("Too low")
	}
	curr := c.prog[i]
	for j := 0; j < 8; j++ {
		copy := c.Copy()
		nVal := (aVal << 3) | j
		copy.regA = nVal
		res := copy.RunII()
		if res == exp {
			return nVal, nil
		}
		if (res & 7) == curr {
			v, e := c.ReverseCalculation(nVal, i-1, exp)
			if e == nil {
				return v, nil
			}
		}
	}
	return 0, errors.New("value not found")
}

func (c *Computer) Copy() *Computer {
	copy := *c
	return &copy
}

func ParseComputer(prog []string) *Computer {
	var regA, regB, regC int
	fmt.Sscanf(prog[0], "Register A: %d", &regA)
	fmt.Sscanf(prog[1], "Register B: %d", &regB)
	fmt.Sscanf(prog[2], "Register C: %d", &regC)
	numerals := strings.Split(strings.Split(prog[4], " ")[1], ",")
	insts := make([]int, len(numerals))
	for k := range numerals {
		v, err := strconv.Atoi(numerals[k])
		if err != nil {
			log.Fatalln("Invalid numeral", err)
		}
		insts[k] = v
	}
	return &Computer{regA: regA, regB: regB, regC: regC, ip: 0, prog: insts}
}

func toOctalInt(v []int) int {
	sum := 0
	for i := range v {
		sum = (v[i] << (i * 3)) | sum
	}
	return sum
}

func BuildSliceString(v []int) string {
	b := strings.Builder{}
	for k := range v {
		if k != 0 {
			b.WriteByte(',')
		}
		b.WriteString(strconv.Itoa(v[k]))
	}
	return b.String()
}

func main() {
	prog := util.InputSlice()
	template := ParseComputer(prog)
	comp := template.Copy()
	debug := comp.Run()
	fmt.Printf("The debug data for computer are \"%s\"\n", BuildSliceString(debug))
	aVal, err := template.ReverseCalculation(0, len(comp.prog)-1, toOctalInt(comp.prog))
	if err == nil {
		fmt.Printf("The value of register A should be %d\n", aVal)
	} else {
		fmt.Println("Couldn't sovle the problem", err)
	}
}