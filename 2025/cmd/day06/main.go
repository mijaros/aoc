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

	"github.com/mijaros/adventofcode/v2025/pkg/util"
)

func init() {
	util.SetIdentifier(6)
}

func parseString(s string) int {
	n, err := strconv.Atoi(s)
	if err != nil {
		panic(err.Error())
	}
	return n
}

type OP byte

const (
	ADD OP = iota
	MUL
)

func ParseOp(s string) OP {
	if s == "*" {
		return MUL
	}
	if s == "+" {
		return ADD
	}
	panic("Invalid OP")
}

func (p OP) Apply(left, right int) int {
	if p == ADD {
		return left + right
	}
	return left * right
}

func ParseCephalopodWorkSheet(in [][]byte) [][]int {
	result := make([][]int, 0)
	it := make([]int, 0)
	for j := range in[0] {
		currNo := make([]byte, 0)
		onlySpace := true
		for i := range in {
			if in[i][j] == ' ' {
				continue
			}
			onlySpace = false
			currNo = append(currNo, in[i][j])
		}
		if onlySpace {
			result = append(result, it)
			it = make([]int, 0)
			continue
		}
		n, err := strconv.Atoi(string(currNo))
		if err != nil {
			panic(err.Error())
		}
		it = append(it, n)
	}
	result = append(result, it)
	return result
}

func GetPartIAnswer(in [][]int, ops []OP) int {
	result := make([]int, len(in[0]))
	copy(result, in[0])
	for _, l := range in[1:] {
		for i := range l {
			result[i] = ops[i].Apply(result[i], l[i])
		}
	}
	if util.Verbose {
		fmt.Println(result)
	}
	sum := 0
	for _, v := range result {
		sum += v
	}
	return sum

}

func GetPartIIAnswer(in [][]int, ops []OP) int {
	res := make([]int, len(in))
	for i := range in {
		res[i] = in[i][0]
		for _, v := range in[i][1:] {
			res[i] = ops[i].Apply(res[i], v)
		}
	}
	sum := 0
	for _, v := range res {
		sum += v
	}
	return sum
}

func main() {
	data := util.InputMatStr()
	data2 := util.InputBytes()
	mat := util.TransformMat(data[:len(data)-1], parseString)
	ops := util.Transform(data[len(data)-1], ParseOp)
	if util.Verbose {
		fmt.Println(mat)
		fmt.Println(data[len(data)-1])
	}
	sum := GetPartIAnswer(mat, ops)
	fmt.Println("The sum in the worksheets sum (Part I answer) is", sum)
	worksheet := ParseCephalopodWorkSheet(data2[:len(data2)-1])
	sum2 := GetPartIIAnswer(worksheet, ops)
	fmt.Println("The sum of corrected worksheet (Part II answer) is", sum2)
}
