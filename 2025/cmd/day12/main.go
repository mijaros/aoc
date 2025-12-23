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
	"fmt"
	"strconv"
	"strings"

	"github.com/mijaros/adventofcode/v2025/pkg/util"
)

func init() {
	util.SetIdentifier(12)
}

type Gift struct {
	layOut []string
	area   int
}

type TreeArea struct {
	width, depth, area int
	presentCounts      []int
}

func (t TreeArea) CanFitArea(gifts []Gift) bool {
	targetArea := 0
	for i, g := range t.presentCounts {
		targetArea += g * gifts[i].area
	}
	return targetArea <= t.area
}

func (t TreeArea) MustFitArea() bool {
	giftGrid := (t.width / 3) * (t.depth / 3)
	gifts := 0
	for _, g := range t.presentCounts {
		gifts += g
	}
	return gifts <= giftGrid
}

func readInt(s string) int {
	n, err := strconv.Atoi(s)
	if err != nil {
		panic("Invalid number")
	}
	return n
}

func ParseInput(in []string) ([]Gift, []TreeArea) {
	gifts := make([]Gift, 6)
	line := 0
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Failed during processing of line %d, %s\n", line, in[line])
			panic(r)
		}
	}()
	for i := range gifts {
		line++
		fill := 0
		for j := 0; j < 3; j++ {
			gifts[i].layOut = append(gifts[i].layOut, in[line])
			for _, c := range []byte(in[line]) {
				if c == '#' {
					fill++
				}
			}
			line++
		}
		gifts[i].area = fill
		line++
	}
	trees := make([]TreeArea, 0)
	for line < len(in) {
		sp := strings.Split(in[line], ":")
		area := util.Transform(strings.Split(strings.Trim(sp[0], " "), "x"), readInt)
		giftCount := util.Transform(strings.Split(strings.Trim(sp[1], " "), " "), readInt)
		t := TreeArea{width: area[0],
			depth:         area[1],
			area:          area[0] * area[1],
			presentCounts: giftCount}
		trees = append(trees, t)
		line++

	}
	return gifts, trees
}

func main() {
	raw := util.InputSlice()
	gifts, trees := ParseInput(raw)

	lowerBound, upperBound := 0, 0
	for _, tree := range trees {
		if tree.MustFitArea() {
			lowerBound++
		}
		if tree.CanFitArea(gifts) {
			upperBound++
		}
	}
	fmt.Printf("At most %d can fit, at least %d must fit.\n", upperBound, lowerBound)
	fmt.Println("The line above is not an exact answer but suffice.")
}
