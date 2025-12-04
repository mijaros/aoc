// Copyright (c) 2025 Miroslav Jaros
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// SPDX-License-Identifier: MIT

package main

import (
	"errors"
	"fmt"

	"github.com/mijaros/adventofcode/v2025/pkg/util"
)

func init() {
	util.SetIdentifier(1)
}

type Direction int

const (
	LEFT Direction = iota
	RIGHT
)

func ParseDir(s byte) (d Direction, e error) {
	switch s {
	case 'L':
		d = LEFT
	case 'R':
		d = RIGHT
	default:
		e = errors.New("Invalid")
	}
	return
}

type Rotation struct {
	dir    Direction
	clicks int
}

func (r Rotation) GetOffset() int {
	res := r.clicks
	if r.dir == LEFT {
		res *= -1
	}
	return res
}

type Dial struct {
	state, code int
}

func NewDial() *Dial {
	d := Dial{state: 50, code: 0}
	return &d
}

func (d *Dial) AddRotation(r Rotation) {
	overFlow := false
	if d.state != 0 {
		overFlow = true
	}

	d.state += r.GetOffset()
	fullC := d.state / 100
	if fullC < 0 {
		fullC *= -1
	}
	if overFlow && d.state <= 0 {
		fullC++
	}
	d.code += fullC
	d.state %= 100
	if d.state < 0 {
		d.state += 100
	}
}

func ParseRotations(in []string) (r []Rotation, err error) {
	result := make([]Rotation, 0)
	for _, k := range in {
		var dirB byte
		var rot, n int
		n, err = fmt.Sscanf(k, "%c%d", &dirB, &rot)
		if n != 2 {
			err = errors.New("Invalid number of conversions")
			return
		}
		if err != nil {
			return
		}
		var dir Direction
		dir, err = ParseDir(dirB)
		if err != nil {
			return
		}
		result = append(result, Rotation{dir: dir, clicks: rot})
	}
	r = result
	return
}

func main() {
	str := util.InputSlice()
	rot, _ := ParseRotations(str)
	dial := NewDial()
	code := 0
	for _, k := range rot {
		dial.AddRotation(k)
		if dial.state == 0 {
			code++
		}
	}
	fmt.Println("Part I code is", code)
	fmt.Println("Part II code is", dial.code)
}
